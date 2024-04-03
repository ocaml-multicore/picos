open Picos

let intr_sig = Sys.sigusr2
let is_intr_sig s = s == intr_sig
let intr_sigs = [ intr_sig ]
let intr_pending = Atomic.make 0

type cancel_at =
  | Cancel_at : {
      time : Mtime.span;
      exn_bt : Exn_bt.t;
      computation : 'a Computation.t;
    }
      -> cancel_at

module Q =
  Psq.Make
    (Int)
    (struct
      type t = cancel_at

      let compare (Cancel_at l) (Cancel_at r) = Mtime.Span.compare l.time r.time
    end)

type return_on =
  | Return_on : {
      file_descr : Picos_fd.t;
      value : 'a;
      computation : 'a Computation.t;
      mutable alive : bool;
    }
      -> return_on

type phase = Continue | Select | Waking_up | Process

type state = {
  phase : phase Atomic.t;
  mutable state : [ `Initial | `Starting | `Alive | `Stopping | `Stopped ];
  mutable exn_bt : Exn_bt.t;
  mutable pipe_inn : Unix.file_descr;
  mutable pipe_out : Unix.file_descr;
  byte : Bytes.t;
  (* *)
  timeouts : Q.t Atomic.t;
  mutable next_id : int;
  (* *)
  new_rd : return_on list ref;
  new_wr : return_on list ref;
  new_ex : return_on list ref;
}

let exit_exn_bt = Exn_bt.get_callstack 0 Exit

let key =
  Picos_domain.DLS.new_key @@ fun () ->
  {
    phase = Atomic.make Continue;
    state = `Initial;
    exn_bt = exit_exn_bt;
    pipe_inn = Unix.stdin;
    pipe_out = Unix.stdin;
    byte = Bytes.create 1;
    timeouts = Atomic.make Q.empty;
    next_id = 0;
    new_rd = ref [];
    new_wr = ref [];
    new_ex = ref [];
  }

let[@poll error] [@inline never] try_transition s from into =
  s.state == from
  && begin
       s.state <- into;
       true
     end

let[@poll error] [@inline never] transition s into =
  let from = s.state in
  s.state <- into;
  from

let rec wakeup s from =
  match Atomic.get s.phase with
  | Process | Waking_up ->
      (* The thread will process the fds and timeouts before next select. *)
      ()
  | Continue ->
      if Atomic.compare_and_set s.phase Continue Process then
        (* We managed to signal the wakeup before the thread was ready to call
           select and the thread will notice this without us needing to write to
           the pipe. *)
        ()
      else
        (* Either the thread called select or another wakeup won the race.  We
           need to retry. *)
        wakeup s from
  | Select ->
      if Atomic.compare_and_set s.phase Select Waking_up then
        if s.state == from then
          (* We are now responsible for writing to the pipe to force the thread to
             exit the select. *)
          let n = Unix.write s.pipe_out s.byte 0 1 in
          assert (n = 1)

type fos = { n : int; unique_fds : Unix.file_descr list; ops : return_on list }

let fos_empty = { n = 1; unique_fds = []; ops = [] }

module Ht = Hashtbl.Make (Picos_fd.Resource)

let rec process_fds ht unique_fds ops = function
  | [] ->
      if unique_fds == [] && ops == [] then fos_empty
      else { n = Ht.length ht; unique_fds; ops }
  | (Return_on r as op) :: ops_todo ->
      if Computation.is_running r.computation then begin
        let file_descr = Picos_fd.unsafe_get r.file_descr in
        match Ht.find ht file_descr with
        | `Return ->
            Picos_fd.decr r.file_descr;
            r.alive <- false;
            Computation.return r.computation r.value;
            process_fds ht unique_fds ops ops_todo
        | `Alive -> process_fds ht unique_fds (op :: ops) ops_todo
        | exception Not_found ->
            Ht.add ht file_descr `Alive;
            process_fds ht (file_descr :: unique_fds) (op :: ops) ops_todo
      end
      else begin
        Picos_fd.decr r.file_descr;
        process_fds ht unique_fds ops ops_todo
      end

let process_fds unique_fds fos new_ops =
  if fos.ops == [] && new_ops == [] then fos_empty
  else
    let ht = Ht.create fos.n in
    unique_fds |> List.iter (fun fd -> Ht.add ht fd `Return);
    let r = process_fds ht [] [] fos.ops in
    if new_ops == [] then r else process_fds ht r.unique_fds r.ops new_ops

let rec process_timeouts s =
  let before = Atomic.get s.timeouts in
  match Q.pop before with
  | None -> -1.0
  | Some ((_, Cancel_at e), after) ->
      let elapsed = Mtime_clock.elapsed () in
      if Mtime.Span.compare e.time elapsed <= 0 then begin
        if Atomic.compare_and_set s.timeouts before after then
          Computation.cancel e.computation e.exn_bt;
        process_timeouts s
      end
      else
        Mtime.Span.to_float_ns (Mtime.Span.abs_diff e.time elapsed)
        *. (1. /. 1_000_000_000.)

let rec select_thread s timeout rd wr ex =
  if s.state == `Alive then
    if Atomic.compare_and_set s.phase Continue Select then
      begin
        try
          Unix.select
            (s.pipe_inn :: rd.unique_fds)
            wr.unique_fds ex.unique_fds timeout
        with Unix.Unix_error (EINTR, _, _) -> ([], [], [])
      end
      |> select_thread_continue s rd wr ex
    else select_thread_continue s rd wr ex ([], [], [])

and select_thread_continue s rd wr ex (rd_fds, wr_fds, ex_fds) =
  begin
    match Atomic.exchange s.phase Continue with
    | Select | Process | Continue -> ()
    | Waking_up ->
        let n = Unix.read s.pipe_inn s.byte 0 1 in
        assert (n = 1)
  end;
  let rd = process_fds rd_fds rd (Picos_thread_atomic.exchange s.new_rd []) in
  let wr = process_fds wr_fds wr (Picos_thread_atomic.exchange s.new_wr []) in
  let ex = process_fds ex_fds ex (Picos_thread_atomic.exchange s.new_ex []) in
  let tos = process_timeouts s in
  let tos =
    let n = Atomic.get intr_pending in
    if n = 0 then tos
    else begin
      Unix.kill (Unix.getpid ()) intr_sig;
      let idle = 0.000_001 (* 1μs *) in
      if tos < 0.0 || idle <= tos then idle else tos
    end
  in
  select_thread s tos rd wr ex

let select_thread s =
  if not Sys.win32 then Thread.sigmask SIG_BLOCK intr_sigs |> ignore;
  begin
    try
      let pipe_inn, pipe_out = Unix.pipe ~cloexec:true () in
      s.pipe_inn <- pipe_inn;
      s.pipe_out <- pipe_out;
      if try_transition s `Starting `Alive then
        select_thread s (-1.0) fos_empty fos_empty fos_empty
    with exn -> s.exn_bt <- Exn_bt.get exn
  end;
  transition s `Stopped |> ignore;
  if s.pipe_inn != Unix.stdin then Unix.close s.pipe_inn;
  if s.pipe_out != Unix.stdin then Unix.close s.pipe_out

let[@inline never] init s =
  if try_transition s `Initial `Starting then begin
    match Thread.create select_thread s with
    | thread ->
        Picos_domain.at_exit @@ fun () ->
        if try_transition s `Alive `Stopping then wakeup s `Stopping;
        Thread.join thread;
        if s.exn_bt != exit_exn_bt then Exn_bt.raise s.exn_bt
    | exception exn ->
        transition s `Stopped |> ignore;
        raise exn
  end;
  while s.state == `Starting do
    Thread.yield ()
  done;
  if s.state != `Alive then
    invalid_arg "Picos_select: domain has been terminated"

let get () =
  let s = Picos_domain.DLS.get key in
  if s.state != `Alive then init s;
  s

(* *)

let[@poll error] [@inline never] next_id t =
  let id = t.next_id in
  t.next_id <- id + 1;
  id

let rec add_timeout s id entry =
  let before = Atomic.get s.timeouts in
  let after = Q.add id entry before in
  if Atomic.compare_and_set s.timeouts before after then
    match Q.min after with
    | Some (id', _) -> if id = id' then wakeup s `Alive
    | None -> ()
  else add_timeout s id entry

let rec remove_action _trigger s id =
  let before = Atomic.get s.timeouts in
  let after = Q.remove id before in
  if not (Atomic.compare_and_set s.timeouts before after) then
    remove_action (Obj.magic ()) s id

let to_deadline ~seconds =
  match Mtime.Span.of_float_ns (seconds *. 1_000_000_000.) with
  | None ->
      invalid_arg
        "Picos_select: seconds should be between 0 to pow(2, 53) nanoseconds"
  | Some span -> Mtime.Span.add (Mtime_clock.elapsed ()) span

let[@alert "-handler"] cancel_after computation ~seconds exn_bt =
  let time = to_deadline ~seconds in
  let entry = Cancel_at { time; exn_bt; computation } in
  let s = get () in
  let id = next_id s in
  add_timeout s id entry;
  let remover = Trigger.from_action s id remove_action in
  if not (Computation.try_attach computation remover) then
    Trigger.signal remover

(* *)

let wakeup_action _trigger s (Return_on r) = if r.alive then wakeup s `Alive

let[@alert "-handler"] rec insert_fd s fds (Return_on r as op) =
  let before = !fds in
  if Computation.is_running r.computation then
    if Picos_thread_atomic.compare_and_set fds before (Return_on r :: before)
    then
      let _ : bool =
        Computation.try_attach r.computation
          (Trigger.from_action s op wakeup_action)
      in
      wakeup s `Alive
    else insert_fd s fds op
  else Picos_fd.decr r.file_descr

let return_on computation file_descr op value =
  Picos_fd.incr file_descr;
  let s = get () in
  insert_fd s
    (match op with `R -> s.new_rd | `W -> s.new_wr | `E -> s.new_ex)
    (Return_on { computation; file_descr; value; alive = true })

let await_on file_descr op =
  let computation = Computation.create () in
  return_on computation file_descr op file_descr;
  try Computation.await computation
  with exn ->
    Computation.cancel computation exit_exn_bt;
    raise exn

(* *)

module Intr = struct
  type intr_status = Cleared | Signaled

  type _ tdt =
    | Nothing : [> `Nothing ] tdt
    | Req : {
        state : state;
        mutable computation : intr_status Computation.t;
      }
        -> [> `Req ] tdt

  type t = T : [< `Nothing | `Req ] tdt -> t [@@unboxed]

  let cleared =
    let computation = Computation.create () in
    Computation.return computation Cleared;
    computation

  let intr_key =
    Picos_tls.new_key @@ fun () : [ `Req ] tdt ->
    Req { state = get (); computation = cleared }

  let handle _ =
    let (Req r) = Picos_tls.get intr_key in
    Computation.return r.computation Signaled

  let intr_action trigger (Req r : [ `Req ] tdt) id =
    match Computation.await r.computation with
    | Cleared ->
        (* No signal needs to be delivered. *)
        remove_action trigger r.state id
    | Signaled ->
        (* Signal was delivered before timeout. *)
        remove_action trigger r.state id;
        if Atomic.fetch_and_add intr_pending 1 = 0 then begin
          (* We need to make sure at least one select thread will keep on
             triggering interrupts. *)
          wakeup r.state `Alive
        end
    | exception Exit ->
        (* The timeout was triggered.  This must have been called from the
           select thread, which will soon trigger an interrupt. *)
        Atomic.incr intr_pending

  let () =
    if not Sys.win32 then begin
      let previously_blocked = Thread.sigmask SIG_BLOCK intr_sigs in
      assert (not (List.exists is_intr_sig previously_blocked));
      let old_behavior = Sys.signal intr_sig (Sys.Signal_handle handle) in
      assert (old_behavior == Signal_default)
    end

  let nothing = T Nothing

  let[@alert "-handler"] req ~seconds =
    if Sys.win32 then
      invalid_arg "Picos_select.Intr is not supported on Windows"
    else begin
      let time = to_deadline ~seconds in
      let (Req r as req) = Picos_tls.get intr_key in
      assert (not (Computation.is_running r.computation));
      let id = next_id r.state in
      let computation = Computation.with_action req id intr_action in
      r.computation <- computation;
      let entry = Cancel_at { time; exn_bt = exit_exn_bt; computation } in
      add_timeout r.state id entry;
      let was_blocked : int list = Thread.sigmask SIG_UNBLOCK intr_sigs in
      assert (List.exists is_intr_sig was_blocked);
      T req
    end

  let clr = function
    | T Nothing -> ()
    | T (Req r) ->
        let was_blocked : int list = Thread.sigmask SIG_BLOCK intr_sigs in
        assert (not (List.exists is_intr_sig was_blocked));
        if not (Computation.try_return r.computation Cleared) then begin
          while
            let count = Atomic.get intr_pending in
            count <= 0
            || not (Atomic.compare_and_set intr_pending count (count - 1))
          do
            Thread.yield ()
          done
        end
end
