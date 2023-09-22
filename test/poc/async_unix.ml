open Picos

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
    mutable pipe_out : Unix.file_descr;
    reading : Awaiter.t list Atomic.t;
    writing : Awaiter.t list Atomic.t;
  }

  let key =
    (* In this case we really want to use DLS rather than TLS. *)
    Domain.DLS.new_key @@ fun () ->
    {
      state = `Init;
      pipe_out =
        (* Unfortunately we cannot safely allocate a pipe here, so we use stdin
           as a dummy value. *)
        Unix.stdin;
      reading = Atomic.make [];
      writing = Atomic.make [];
    }

  let[@poll error] try_lock s =
    s.state == `Init
    && begin
         s.state <- `Locked;
         true
       end

  let needs_init s = s.state != `Alive

  let[@poll error] unlock s pipe_out =
    s.pipe_out <- pipe_out;
    s.state <- `Alive

  let wakeup s =
    let n = Unix.write s.pipe_out (Bytes.create 1) 0 1 in
    assert (n = 1)

  let rec init s =
    (* DLS initialization may be run multiple times, so we perform more involved
       initialization here. *)
    if try_lock s then begin
      (* The pipe is used to wake up the select after changing the lists of
         reading and writing file descriptors. *)
      let pipe_inn, pipe_out = Unix.pipe ~cloexec:true () in
      unlock s pipe_out;
      let t =
        ()
        |> Thread.create @@ fun () ->
           (* This is the IO select loop that performs select and then wakes up
              fibers blocked on IO. *)
           while s.state != `Dead do
             let rs, ws, _ =
               Unix.select
                 (pipe_inn
                 :: List.map Awaiter.file_descr_of (Atomic.get s.reading))
                 (List.map Awaiter.file_descr_of (Atomic.get s.writing))
                 [] (-1.0)
             in
             List.iter
               (Awaiter.signal_or_wakeup pipe_inn (Atomic.get s.reading))
               rs;
             List.iter (Awaiter.signal (Atomic.get s.writing)) ws;
             Atomic.modify s.reading (List.fold_right Awaiter.reject rs);
             Atomic.modify s.writing (List.fold_right Awaiter.reject ws)
           done;
           Unix.close pipe_inn;
           Unix.close pipe_out
      in
      Domain.at_exit @@ fun () ->
      s.state <- `Dead;
      wakeup s;
      Thread.join t
    end
    else if needs_init s then begin
      Thread.yield ();
      init s
    end

  let get () =
    let s = Domain.DLS.get key in
    if needs_init s then init s;
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
  let s = Async.get () in
  Async.await s s.reading file_descr;
  Unix.read file_descr bytes pos len

let write file_descr bytes pos len =
  let s = Async.get () in
  Async.await s s.writing file_descr;
  Unix.write file_descr bytes pos len

let accept ?cloexec file_descr =
  let s = Async.get () in
  Async.await s s.reading file_descr;
  Unix.accept ?cloexec file_descr
