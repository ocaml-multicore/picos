open Picos

let handle_sigchld_bit = 0b01
let select_thread_running_on_main_domain_bit = 0b10

type config = {
  mutable bits : int;
  mutable intr_sig : int;
  mutable intr_sigs : int list;
}

let config = { bits = 0; intr_sig = 0; intr_sigs = [] }

(* *)

type return =
  | Return : {
      value : 'a;
      computation : 'a Computation.t;
      mutable alive : bool;
    }
      -> return

(** We use random numbers as keys for the awaiters. *)
module RandomInt = struct
  type t = int

  let equal = Int.equal
  let hash = Fun.id
end

let chld_awaiters = Picos_htbl.create ~hashed_type:(module RandomInt) ()

(* *)

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

type intr_status = Cleared | Signaled

type _ tdt =
  | Nothing : [> `Nothing ] tdt
  | Req : {
      state : state;
      mutable unused : bool;
      mutable computation : intr_status Computation.t;
    }
      -> [> `Req ] tdt

type req = R : [< `Nothing | `Req ] tdt -> req [@@unboxed]
type counter_state = { value : int; req : req }

let intr_pending = Atomic.make { value = 0; req = R Nothing }
let exit_exn_bt = Exn_bt.get_callstack 0 Exit

let cleared =
  let computation = Computation.create () in
  Computation.return computation Cleared;
  computation

let intr_key =
  Picos_thread.TLS.new_key @@ fun () : [ `Req ] tdt ->
  invalid_arg "has not been configured"

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
          (* We are now responsible for writing to the pipe to force the thread
             to exit the select. *)
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
  if s.state == `Alive then (
    let rd_fds, wr_fds, ex_fds =
      if Atomic.compare_and_set s.phase Continue Select then begin
        try
          Unix.select
            (s.pipe_inn :: rd.unique_fds)
            wr.unique_fds ex.unique_fds timeout
        with Unix.Unix_error (EINTR, _, _) -> ([], [], [])
      end
      else ([], [], [])
    in
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
    let timeout = process_timeouts s in
    let timeout =
      let state = Atomic.get intr_pending in
      if state.value = 0 then timeout
      else begin
        assert (0 < state.value);
        Unix.kill (Unix.getpid ()) config.intr_sig;
        let idle = 0.000_001 (* 1μs *) in
        if timeout < 0.0 || idle <= timeout then idle else timeout
      end
    in
    select_thread s timeout rd wr ex)

let select_thread s =
  if Picos_domain.is_main_domain () then
    config.bits <- select_thread_running_on_main_domain_bit lor config.bits;
  if not Sys.win32 then
    Thread.sigmask SIG_BLOCK (Sys.sigchld :: config.intr_sigs) |> ignore;
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

let[@poll error] [@inline never] try_configure ~intr_sig ~intr_sigs
    ~handle_sigchld =
  config.intr_sigs == []
  && begin
       config.bits <- Bool.to_int handle_sigchld;
       config.intr_sig <- intr_sig;
       config.intr_sigs <- intr_sigs;
       true
     end

let is_sigchld signum = signum = Sys.sigchld
let is_intr_sig signum = signum = config.intr_sig

let rec configure ?(intr_sig = Sys.sigusr2) ?(handle_sigchld = true) () =
  if not (Picos_thread.is_main_thread ()) then
    invalid_arg "must be called from the main thread on the main domain";
  assert (Sys.sigabrt = -1 && Sys.sigxfsz < Sys.sigabrt);
  if intr_sig < Sys.sigxfsz || 0 <= intr_sig || intr_sig = Sys.sigchld then
    invalid_arg "invalid interrupt signal number";
  if not (try_configure ~intr_sig ~intr_sigs:[ intr_sig ] ~handle_sigchld) then
    invalid_arg "already configured";

  if not Sys.win32 then begin
    if config.bits land handle_sigchld_bit <> 0 then begin
      let previously_blocked = Thread.sigmask SIG_UNBLOCK [ Sys.sigchld ] in
      assert (not (List.exists is_sigchld previously_blocked));
      let old_behavior =
        Sys.signal Sys.sigchld (Sys.Signal_handle handle_signal)
      in
      assert (old_behavior == Signal_default)
    end;
    begin
      let previously_blocked = Thread.sigmask SIG_BLOCK config.intr_sigs in
      assert (not (List.exists is_intr_sig previously_blocked));
      let old_behavior =
        Sys.signal config.intr_sig (Sys.Signal_handle handle_signal)
      in
      assert (old_behavior == Signal_default)
    end
  end

and reconfigure_signal_handlers () =
  if not (Picos_thread.is_main_thread ()) then
    invalid_arg "must be called from the main thread on the main domain";
  if not Sys.win32 then begin
    if config.bits land handle_sigchld_bit <> 0 then begin
      Thread.sigmask SIG_UNBLOCK [ Sys.sigchld ] |> ignore;
      Sys.signal Sys.sigchld (Sys.Signal_handle handle_signal) |> ignore
    end;
    begin
      Thread.sigmask SIG_BLOCK config.intr_sigs |> ignore;
      Sys.signal config.intr_sig (Sys.Signal_handle handle_signal) |> ignore
    end
  end

and handle_signal signal =
  if signal = Sys.sigchld then begin
    Picos_htbl.remove_all chld_awaiters
    |> Seq.iter @@ fun (_, Return r) ->
       r.alive <- false;
       Computation.return r.computation r.value
  end
  else if signal = config.intr_sig then
    let (Req r) = Picos_thread.TLS.get intr_key in
    Computation.return r.computation Signaled

let check_configured () = if config.intr_sigs = [] then configure ()

let[@inline never] init s =
  check_configured ();
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
  if s.state != `Alive then invalid_arg "domain has been terminated"

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
  | None -> invalid_arg "seconds should be between 0 to pow(2, 53) nanoseconds"
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
  type t = req

  let[@inline] use = function R Nothing -> () | R (Req r) -> r.unused <- false

  (** This is used to ensure that the [intr_pending] counter is incremented
      exactly once before the counter is decremented. *)
  let rec incr_once (Req r as req : [ `Req ] tdt) backoff =
    let before = Atomic.get intr_pending in
    (* [intr_pending] must be read before [r.unused]! *)
    r.unused && before.req != R req
    && begin
         use before.req;
         let after = { value = before.value + 1; req = R req } in
         if Atomic.compare_and_set intr_pending before after then
           after.value = 1
         else incr_once req (Backoff.once backoff)
       end

  let intr_action trigger (Req r as req : [ `Req ] tdt) id =
    match Computation.await r.computation with
    | Cleared ->
        (* No signal needs to be delivered. *)
        remove_action trigger r.state id
    | Signaled ->
        (* Signal was delivered before timeout. *)
        remove_action trigger r.state id;
        if incr_once req Backoff.default then
          (* We need to make sure at least one select thread will keep on
             triggering interrupts. *)
          wakeup r.state `Alive
    | exception Exit ->
        (* The timeout was triggered.  This must have been called from the
           select thread, which will soon trigger an interrupt. *)
        let _ : bool = incr_once req Backoff.default in
        ()

  let nothing = R Nothing

  let[@alert "-handler"] req ~seconds =
    if Sys.win32 then invalid_arg "not supported on Windows"
    else begin
      let time = to_deadline ~seconds in
      (* assert (not (Computation.is_running r.computation)); *)
      let state = get () in
      let id = next_id state in
      let (Req r as req : [ `Req ] tdt) =
        Req { state; unused = true; computation = cleared }
      in
      let computation = Computation.with_action req id intr_action in
      r.computation <- computation;
      Picos_thread.TLS.set intr_key req;
      let entry = Cancel_at { time; exn_bt = exit_exn_bt; computation } in
      add_timeout state id entry;
      let was_blocked : int list =
        Thread.sigmask SIG_UNBLOCK config.intr_sigs
      in
      assert (List.exists is_intr_sig was_blocked);
      R req
    end

  let rec decr backoff =
    let before = Atomic.get intr_pending in
    use before.req;
    let after = { value = before.value - 1; req = R Nothing } in
    assert (0 <= after.value);
    if not (Atomic.compare_and_set intr_pending before after) then
      decr (Backoff.once backoff)

  let clr = function
    | R Nothing -> ()
    | R (Req r as req) ->
        let was_blocked : int list =
          Thread.sigmask SIG_BLOCK config.intr_sigs
        in
        assert (not (List.exists is_intr_sig was_blocked));
        if not (Computation.try_return r.computation Cleared) then begin
          let _ : bool = incr_once req Backoff.default in
          (* We ensure that the associated increment has been done before we
             decrement so that the [intr_pending] counter is never too low. *)
          decr Backoff.default
        end
end

(* *)

let rec insert return =
  let id = Random.bits () in
  if Picos_htbl.try_add chld_awaiters id return then id else insert return

let[@alert "-handler"] return_on_sigchld computation value =
  let return = Return { value; computation; alive = true } in
  let id = insert return in
  let remover =
    Trigger.from_action id return @@ fun _trigger id (Return this_r as this) ->
    if this_r.alive then begin
      this_r.alive <- false;
      (* It should be extremely rare, but possible, that the return was already
         removed and another added just at this point and so we must account for
         the possibility and make sure that whatever we remove is completed. *)
      match Picos_htbl.remove_exn chld_awaiters id with
      | Return that_r as that ->
          if this != that then
            Computation.return that_r.computation that_r.value
      | exception Not_found -> ()
    end
  in
  if not (Computation.try_attach computation remover) then
    Trigger.signal remover
