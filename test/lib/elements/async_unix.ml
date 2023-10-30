open Picos
open Foundation

module Async = struct
  module Awaiter = struct
    type t = { file_descr : Unix.file_descr; trigger : Trigger.as_signal }

    let file_descr_of t = t.file_descr

    let rec signal aws file_descr =
      match aws with
      | [] -> ()
      | aw :: aws ->
          if aw.file_descr == file_descr then Trigger.signal aw.trigger
          else signal aws file_descr

    let signal_or_wakeup wakeup aws file_descr =
      if file_descr == wakeup then begin
        let n = Unix.read file_descr (Bytes.create 1) 0 1 in
        assert (n = 1)
      end
      else signal aws file_descr

    let reject file_descr = List.filter (fun aw -> aw.file_descr != file_descr)
  end

  type state = {
    mutable state : [ `Init | `Locked | `Alive | `Dead ];
    mutable pipe_inn : Unix.file_descr;
    mutable pipe_out : Unix.file_descr;
    mutable exn_bt : Exn_bt.t;
    reading : Awaiter.t list Atomic.t;
    writing : Awaiter.t list Atomic.t;
  }

  let key =
    (* In this case we really want to use DLS rather than TLS. *)
    (* Unfortunately we cannot safely allocate a pipe here, so we use stdin
       as a dummy value. *)
    let dummy_pipe = Unix.stdin in
    DLS.new_key @@ fun () ->
    {
      state = `Init;
      pipe_inn = dummy_pipe;
      pipe_out = dummy_pipe;
      exn_bt = Exn_bt.get_callstack 0 Exit;
      reading = Atomic.make [];
      writing = Atomic.make [];
    }

  let[@poll error] [@inline never] try_lock s =
    s.state == `Init
    && begin
         s.state <- `Locked;
         true
       end

  let[@poll error] [@inline never] unlock s state = s.state <- state

  let wakeup s =
    let n = Unix.write s.pipe_out (Bytes.create 1) 0 1 in
    assert (n = 1)

  let io_thread s =
    begin
      try
        (* The pipe is used to wake up the select after changing the lists of
           reading and writing file descriptors. *)
        let pipe_inn, pipe_out = Unix.pipe ~cloexec:true () in
        s.pipe_inn <- pipe_inn;
        s.pipe_out <- pipe_out;
        unlock s `Alive;
        (* This is the IO select loop that performs select and then wakes up
           fibers blocked on IO. *)
        while s.state == `Alive do
          let rs, ws, _ =
            Unix.select
              (s.pipe_inn
              :: List.map Awaiter.file_descr_of (Atomic.get s.reading))
              (List.map Awaiter.file_descr_of (Atomic.get s.writing))
              [] (-1.0)
          in
          List.iter
            (Awaiter.signal_or_wakeup s.pipe_inn (Atomic.get s.reading))
            rs;
          List.iter (Awaiter.signal (Atomic.get s.writing)) ws;
          Atomic.modify s.reading (List.fold_right Awaiter.reject rs);
          Atomic.modify s.writing (List.fold_right Awaiter.reject ws)
        done
      with exn -> s.exn_bt <- Exn_bt.get exn
    end;
    unlock s `Dead;
    if s.pipe_inn != Unix.stdin then Unix.close s.pipe_inn;
    if s.pipe_out != Unix.stdin then Unix.close s.pipe_out

  let start s =
    (* DLS initialization may be run multiple times, so we perform more involved
       initialization here. *)
    match Thread.create io_thread s with
    | thread ->
        Domain.at_exit @@ fun () ->
        unlock s `Dead;
        wakeup s;
        Thread.join thread;
        if s.exn_bt.exn != Exit then Exn_bt.raise s.exn_bt
    | exception exn ->
        unlock s `Dead;
        raise exn

  let wait s =
    while s.state == `Locked do
      Thread.yield ()
    done;
    if s.state != `Alive then
      invalid_arg "Async_unix: domain has been terminated"

  let init s =
    if try_lock s then start s;
    wait s

  let get () =
    let s = DLS.get key in
    if s.state != `Alive then init s;
    s

  let await s r file_descr =
    let trigger = Trigger.create () in
    let awaiter = Awaiter.{ file_descr; trigger :> Trigger.as_signal } in
    Atomic.modify r (List.cons awaiter);
    wakeup s;
    match Trigger.await trigger with
    | None -> ()
    | Some exn_bt ->
        Atomic.modify r (List.drop_first awaiter);
        Exn_bt.raise exn_bt
end

include Unix

let read file_descr bytes pos len =
  match Unix.read file_descr bytes pos len with
  | result -> result
  | exception Unix.Unix_error ((EAGAIN | EWOULDBLOCK), _, _) ->
      let s = Async.get () in
      Async.await s s.reading file_descr;
      Unix.read file_descr bytes pos len

let write file_descr bytes pos len =
  match Unix.write file_descr bytes pos len with
  | result -> result
  | exception Unix.Unix_error ((EAGAIN | EWOULDBLOCK), _, _) ->
      let s = Async.get () in
      Async.await s s.writing file_descr;
      Unix.write file_descr bytes pos len

let accept ?cloexec file_descr =
  match Unix.accept ?cloexec file_descr with
  | result -> result
  | exception Unix.Unix_error ((EAGAIN | EWOULDBLOCK), _, _) ->
      let s = Async.get () in
      Async.await s s.reading file_descr;
      Unix.accept ?cloexec file_descr
