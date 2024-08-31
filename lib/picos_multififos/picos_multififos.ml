open Picos

let[@inline never] quota_non_positive () = invalid_arg "quota must be positive"
let[@inline never] already_running () = invalid_arg "already running"
let[@inline never] not_worker () = invalid_arg "not a worker thread"

type ready =
  | Spawn of Fiber.t * (Fiber.t -> unit)
  | Continue of Fiber.t * (unit, unit) Effect.Deep.continuation
  | Resume of Fiber.t * (Exn_bt.t option, unit) Effect.Deep.continuation
  | Return of Fiber.Maybe.t * (unit, unit) Effect.Deep.continuation

type t = {
  mutable num_waiters_non_zero : bool;
  num_waiters : int ref;
  num_started : int Atomic.t;
  mutex : Mutex.t;
  condition : Condition.t;
  handler : (unit, unit) Effect.Deep.handler;
  quota : int;
  mutable run : bool;
  mutable threads : [ `Nothing | `Per_thread ] tdt array;
  mutable threads_num : int;
}

and _ tdt =
  | Nothing : [> `Nothing ] tdt
  | Per_thread : {
      ready : ready Picos_mpmcq.t;
      resume :
        Trigger.t ->
        Fiber.t ->
        (Exn_bt.t option, unit) Effect.Deep.continuation ->
        unit;
      return : ((unit, unit) Effect.Deep.continuation -> unit) option;
      discontinue : ((unit, unit) Effect.Deep.continuation -> unit) option;
      context : t;
      mutable index : int;
      mutable num_started : int;
      mutable num_stopped : int;
      mutable fiber : Fiber.Maybe.t;
      mutable remaining_quota : int;
    }
      -> [> `Per_thread ] tdt

and per_thread = [ `Per_thread ] tdt

let per_thread_key = Picos_thread.TLS.create ()

let[@inline] get_per_thread () : per_thread =
  match Picos_thread.TLS.get_exn per_thread_key with
  | Nothing -> not_worker ()
  | Per_thread _ as pt -> pt

let get_thread t i : per_thread =
  match Array.unsafe_get t.threads i with
  | Nothing -> not_worker ()
  | Per_thread _ as pt -> pt

let any_fibers_alive t =
  (* We read the number of stopped fibers first. *)
  let stopped = ref 0 in
  for i = 0 to t.threads_num - 1 do
    let (Per_thread p) = get_thread t i in
    stopped := !stopped + p.num_stopped
  done;
  (* Then we read the number of started fibers.

     The [Atomic.get] below provides a fence that ensures we really read the
     number of started fibers after reading the number of stopped fibers. *)
  let started = ref (Atomic.get t.num_started) in
  for i = 0 to t.threads_num - 1 do
    let (Per_thread p) = get_thread t i in
    started := !started + p.num_started
  done;
  (* Is the difference positive? *)
  0 < !started - !stopped

let rec any_fibers_ready t i =
  0 <= i
  &&
  let (Per_thread p) = get_thread t i in
  Picos_mpmcq.length p.ready != 0 || any_fibers_ready t (i - 1)

let any_fibers_ready t = any_fibers_ready t (t.threads_num - 1)

let next_index t i =
  let i = i + 1 in
  if i < t.threads_num then i else 0

let exec ready (Per_thread p : per_thread) t =
  p.remaining_quota <- t.quota;
  match ready with
  | Spawn (fiber, main) ->
      p.fiber <- Fiber.Maybe.of_fiber fiber;
      Effect.Deep.match_with main fiber t.handler
  | Return (fiber, k) ->
      p.fiber <- fiber;
      Effect.Deep.continue k ()
  | Continue (fiber, k) ->
      p.fiber <- Fiber.Maybe.of_fiber fiber;
      Fiber.continue fiber k ()
  | Resume (fiber, k) ->
      p.fiber <- Fiber.Maybe.of_fiber fiber;
      Fiber.resume fiber k

let rec next (Per_thread p as pt : per_thread) =
  match Picos_mpmcq.pop_exn p.ready with
  | ready ->
      let t = p.context in
      if t.num_waiters_non_zero && Picos_mpmcq.length p.ready != 0 then begin
        Mutex.lock t.mutex;
        Mutex.unlock t.mutex;
        Condition.signal t.condition
      end;
      exec ready pt t
  | exception Picos_mpmcq.Empty ->
      p.fiber <- Fiber.Maybe.nothing;
      let t = p.context in
      try_steal pt t (next_index t p.index)

and try_steal (Per_thread p as pt : per_thread) t i =
  if p.index <> i then begin
    let (Per_thread other_p) = get_thread t i in
    match Picos_mpmcq.pop_exn other_p.ready with
    | ready -> exec ready pt t
    | exception Picos_mpmcq.Empty -> try_steal pt t (next_index t i)
  end
  else wait pt t

and wait (pt : per_thread) t =
  if any_fibers_alive t then begin
    Mutex.lock t.mutex;
    if (not (any_fibers_ready t)) && any_fibers_alive t then begin
      let n = !(t.num_waiters) + 1 in
      t.num_waiters := n;
      if n = 1 then t.num_waiters_non_zero <- true;
      match Condition.wait t.condition t.mutex with
      | () ->
          let n = !(t.num_waiters) - 1 in
          t.num_waiters := n;
          if n = 0 then t.num_waiters_non_zero <- false;
          Mutex.unlock t.mutex;
          next pt
      | exception async_exn ->
          let n = !(t.num_waiters) - 1 in
          t.num_waiters := n;
          if n = 0 then t.num_waiters_non_zero <- false;
          Mutex.unlock t.mutex;
          raise async_exn
    end
    else begin
      Mutex.unlock t.mutex;
      next pt
    end
  end
  else begin
    Mutex.lock t.mutex;
    Mutex.unlock t.mutex;
    Condition.broadcast t.condition
  end

let default_fatal_exn_handler exn =
  prerr_string "Fatal error: exception ";
  prerr_string (Printexc.to_string exn);
  prerr_char '\n';
  Printexc.print_backtrace stderr;
  flush stderr;
  exit 2

let per_thread context =
  let ready = Picos_mpmcq.create ~padded:true () in
  let rec pt : per_thread =
    Per_thread
      {
        ready;
        resume;
        return;
        discontinue;
        context;
        index = 0;
        num_started = 0;
        num_stopped = 0;
        fiber = Fiber.Maybe.nothing;
        remaining_quota = 0;
      }
  and resume trigger fiber k =
    let resume = Resume (fiber, k) in
    let (Per_thread p_original) = pt in
    match Picos_thread.TLS.get_exn per_thread_key with
    | Per_thread p_current when p_original.context == p_current.context ->
        (* We are running on a thread of this scheduler - no wakup needed. *)
        if Fiber.unsuspend fiber trigger then
          Picos_mpmcq.push p_current.ready resume
        else Picos_mpmcq.push_head p_current.ready resume
    | _ | (exception Picos_thread.TLS.Not_set) ->
        (* We are running on a foreign thread - wake up may be needed. *)
        if Fiber.unsuspend fiber trigger then
          Picos_mpmcq.push p_original.ready resume
        else Picos_mpmcq.push_head p_original.ready resume;
        let t = p_original.context in
        let non_zero =
          match Mutex.lock t.mutex with
          | () ->
              let non_zero = t.num_waiters_non_zero in
              Mutex.unlock t.mutex;
              non_zero
          | exception Sys_error _ -> false
        in
        if non_zero then Condition.signal t.condition
  and return =
    Some
      (fun k ->
        let (Per_thread p) = pt in
        let remaining_quota = p.remaining_quota - 1 in
        if 0 < remaining_quota then begin
          p.remaining_quota <- remaining_quota;
          Effect.Deep.continue k ()
        end
        else begin
          Picos_mpmcq.push p.ready (Return (p.fiber, k));
          next pt
        end)
  and discontinue =
    Some
      (fun k ->
        let (Per_thread p) = pt in
        let fiber = Fiber.Maybe.to_fiber p.fiber in
        Fiber.continue fiber k ())
  in
  pt

let with_per_thread t fn =
  let (Per_thread new_p as new_pt) = per_thread t in
  begin
    Mutex.lock t.mutex;
    match
      if Array.length t.threads = t.threads_num then begin
        t.threads <-
          Array.init
            ((t.threads_num * 2) + 1)
            (fun i ->
              if i < t.threads_num then Array.unsafe_get t.threads i
              else Nothing)
      end;
      new_p.index <- t.threads_num;
      Array.unsafe_set t.threads t.threads_num new_pt;
      if t.threads_num = 0 then Atomic.incr t.num_started
      else Multicore_magic.fence t.num_started;
      t.threads_num <- t.threads_num + 1
    with
    | () -> Mutex.unlock t.mutex
    | exception exn ->
        Mutex.unlock t.mutex;
        raise exn
  end;
  let old_p =
    try Picos_thread.TLS.get_exn per_thread_key
    with Picos_thread.TLS.Not_set -> Nothing
  in
  Picos_thread.TLS.set per_thread_key new_pt;
  match fn (new_pt : per_thread) with
  | value ->
      (* TODO: maybe remove from [t.threads]? *)
      Picos_thread.TLS.set per_thread_key old_p;
      value
  | exception exn ->
      Picos_thread.TLS.set per_thread_key old_p;
      raise exn

let current =
  Some
    (fun k ->
      let (Per_thread p) = get_per_thread () in
      let fiber = Fiber.Maybe.to_fiber p.fiber in
      Effect.Deep.continue k fiber)

let yield =
  Some
    (fun k ->
      let (Per_thread p as pt) = get_per_thread () in
      let fiber = Fiber.Maybe.to_fiber p.fiber in
      Picos_mpmcq.push p.ready (Continue (fiber, k));
      next pt)

let[@alert "-handler"] effc :
    type a. a Effect.t -> ((a, _) Effect.Deep.continuation -> _) option =
  function
  | Fiber.Current -> current
  | Fiber.Spawn r ->
      let (Per_thread p) = get_per_thread () in
      let fiber = Fiber.Maybe.to_fiber p.fiber in
      if Fiber.is_canceled fiber then p.discontinue
      else begin
        p.num_started <- p.num_started + 1;
        (* The queue [push] includes a full fence, which means the increment
           of [num_started] will happen before increment of [num_stopped]. *)
        Picos_mpmcq.push p.ready (Spawn (r.fiber, r.main));
        let t = p.context in
        if t.num_waiters_non_zero then begin
          Mutex.lock t.mutex;
          Mutex.unlock t.mutex;
          Condition.signal t.condition
        end;
        p.return
      end
  | Fiber.Yield -> yield
  | Computation.Cancel_after r -> begin
      let (Per_thread p) = get_per_thread () in
      let fiber = Fiber.Maybe.to_fiber p.fiber in
      if Fiber.is_canceled fiber then p.discontinue
      else
        match Select.cancel_after r.computation ~seconds:r.seconds r.exn_bt with
        | () -> p.return
        | exception exn ->
            let exn_bt = Exn_bt.get exn in
            Some (fun k -> Exn_bt.discontinue k exn_bt)
    end
  | Trigger.Await trigger ->
      Some
        (fun k ->
          let (Per_thread p as pt) = get_per_thread () in
          let fiber = Fiber.Maybe.to_fiber p.fiber in
          if Fiber.try_suspend fiber trigger fiber k p.resume then next pt
          else
            let remaining_quota = p.remaining_quota - 1 in
            if 0 < remaining_quota then begin
              p.remaining_quota <- remaining_quota;
              Fiber.resume fiber k
            end
            else begin
              Picos_mpmcq.push p.ready (Resume (fiber, k));
              next pt
            end)
  | _ -> None

let retc () =
  let (Per_thread p as pt) = get_per_thread () in
  p.num_stopped <- p.num_stopped + 1;
  next pt

let context ?quota ?fatal_exn_handler () =
  let quota =
    match quota with
    | None -> Int.max_int
    | Some quota ->
        if quota <= 0 then quota_non_positive ();
        quota
  in
  let exnc =
    match fatal_exn_handler with
    | None -> default_fatal_exn_handler
    | Some handler ->
        fun exn ->
          handler exn;
          raise exn
  in
  Select.check_configured ();
  let mutex = Mutex.create ()
  and condition = Condition.create ()
  and num_waiters = ref 0 |> Multicore_magic.copy_as_padded
  and num_started = Atomic.make 0 |> Multicore_magic.copy_as_padded in
  let rec context =
    {
      num_waiters_non_zero = false;
      num_waiters;
      num_started;
      mutex;
      condition;
      handler;
      quota;
      run = false;
      threads = Array.make 15 Nothing;
      threads_num = 0;
    }
  and handler = { retc; exnc; effc } in
  context

let runner_on_this_thread t =
  Select.check_configured ();
  with_per_thread t next

let run_fiber ?context:t_opt fiber main =
  let t = match t_opt with None -> context () | Some t -> t in
  with_per_thread t @@ fun (Per_thread p as pt) ->
  Mutex.lock t.mutex;
  if t.run then begin
    Mutex.unlock t.mutex;
    already_running ()
  end
  else begin
    t.run <- true;
    Mutex.unlock t.mutex
  end;
  Picos_mpmcq.push p.ready (Spawn (fiber, main));
  next pt

let run ?context ?(forbid = false) main =
  let computation = Computation.create ~mode:`LIFO () in
  let fiber = Fiber.create ~forbid computation in
  let main _ = Computation.capture computation main () in
  run_fiber ?context fiber main;
  Computation.await computation

let rec run_fiber_on n fiber main runner_main context =
  if n <= 1 then run_fiber ~context fiber main
  else
    let runner =
      try Domain.spawn runner_main
      with exn ->
        let bt = Printexc.get_raw_backtrace () in
        run ~context Fun.id;
        Printexc.raise_with_backtrace exn bt
    in
    match run_fiber_on (n - 1) fiber main runner_main context with
    | result ->
        Option.iter Exn_bt.raise (Domain.join runner);
        result
    | exception exn ->
        let bt = Printexc.get_raw_backtrace () in
        Option.iter Exn_bt.raise (Domain.join runner);
        Printexc.raise_with_backtrace exn bt

let run_fiber_on ?quota ?fatal_exn_handler ~n_domains fiber main =
  if n_domains < 1 then invalid_arg "n_domains must be positive";
  let context = context ?quota ?fatal_exn_handler () in
  let runner_main =
    if n_domains = 1 then fun () -> None
    else
      let bt_status = Printexc.backtrace_status () in
      fun () ->
        Printexc.record_backtrace bt_status;
        match runner_on_this_thread context with
        | () -> None
        | exception exn -> Some (Exn_bt.get exn)
  in
  run_fiber_on n_domains fiber main runner_main context

let run_on ?quota ?fatal_exn_handler ~n_domains ?(forbid = false) main =
  let computation = Computation.create ~mode:`LIFO () in
  let fiber = Fiber.create ~forbid computation in
  let main _ = Computation.capture computation main () in
  run_fiber_on ?quota ?fatal_exn_handler ~n_domains fiber main;
  Computation.await computation
