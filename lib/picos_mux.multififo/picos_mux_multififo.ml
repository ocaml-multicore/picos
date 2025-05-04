open Picos

let[@inline never] heartbeat_delay_out_of_range _ =
  invalid_arg "heartbeat_delay should be between 0 and 1 seconds"

let[@inline never] heartbeat_rounds_negative _ =
  invalid_arg "heartbeat_rounds must be non-negative"

let[@inline never] quota_non_positive _ = invalid_arg "quota must be positive"
let[@inline never] already_running () = invalid_arg "already running"
let[@inline never] not_worker _ = invalid_arg "not a worker thread"

module Mpmcq = Picos_aux_mpmcq

type ready =
  | Spawn of Fiber.t * (Fiber.t -> unit)
  | Continue of Fiber.t * (unit, unit) Effect.Deep.continuation
  | Resume of
      Fiber.t
      * ((exn * Printexc.raw_backtrace) option, unit) Effect.Deep.continuation
  | Return of Fiber.t * (unit, unit) Effect.Deep.continuation

let state_running = 1 lsl 0
let state_idlers = 1 lsl 1
let state_arrhytmia = 1 lsl 2
let state_killed = 1 lsl 3

type t = {
  mutable state : int;
  num_started : int Atomic.t;
  mutex : Mutex.t;
  worker_condition : Condition.t;
  heartbeat_condition : Condition.t;
  heartbeat_delay : float;
  heartbeat_rounds : int;
  handler : (unit, unit) Effect.Deep.handler;
  quota : int;
  mutable threads : [ `Nothing | `Per_thread ] tdt array;
  mutable threads_num : int;
}

and _ tdt =
  | Nothing : [> `Nothing ] tdt
  | Per_thread : {
      ready : ready Mpmcq.t;
      mutable resume :
        Trigger.t ->
        Fiber.t ->
        ((exn * Printexc.raw_backtrace) option, unit) Effect.Deep.continuation ->
        unit;
      mutable return : ((unit, unit) Effect.Deep.continuation -> unit) option;
      mutable discontinue :
        ((unit, unit) Effect.Deep.continuation -> unit) option;
      mutable current :
        ((Fiber.t, unit) Effect.Deep.continuation -> unit) option;
      mutable yield : ((unit, unit) Effect.Deep.continuation -> unit) option;
      context : t;
      mutable index : int;
      mutable num_started : int;
      mutable num_stopped : int;
      mutable fiber : Fiber.Maybe.t;
      mutable remaining_quota : int;
      mutable countdown_to_steal : int;
    }
      -> [> `Per_thread ] tdt

and per_thread = [ `Per_thread ] tdt

let kill t =
  if t.state < state_killed then begin
    Mutex.lock t.mutex;
    let state = t.state in
    if state != state lor state_killed then t.state <- state lor state_killed;
    Mutex.unlock t.mutex;
    Condition.broadcast t.heartbeat_condition;
    Condition.broadcast t.worker_condition
  end

let per_thread_key = Picos_thread.TLS.create ()

let[@inline] get_per_thread () : per_thread =
  match Picos_thread.TLS.get_exn per_thread_key with
  | Nothing as any -> not_worker any
  | Per_thread _ as pt -> pt

let any_fibers_alive t =
  (* We read the number of stopped fibers first. *)
  let stopped = ref 0 in
  begin
    let threads_num = t.threads_num in
    let threads = t.threads in
    for i = 0 to threads_num - 1 do
      match Array.unsafe_get threads i with
      | Nothing -> ()
      | Per_thread p -> stopped := !stopped + p.num_stopped
    done
  end;
  (* Then we read the number of started fibers.

     The [Atomic.get] below provides a fence that ensures we really read the
     number of started fibers after reading the number of stopped fibers. *)
  let started = ref (Atomic.get t.num_started) in
  begin
    let threads_num = t.threads_num in
    let threads = t.threads in
    for i = 0 to threads_num - 1 do
      match Array.unsafe_get threads i with
      | Nothing -> ()
      | Per_thread p -> started := !started + p.num_started
    done
  end;
  (* Is the difference positive? *)
  0 < !started - !stopped

let rec any_fibers_ready t i =
  0 <= i
  &&
  match Array.unsafe_get t.threads i with
  | Nothing -> any_fibers_ready t (i - 1)
  | Per_thread p -> Mpmcq.length p.ready != 0 || any_fibers_ready t (i - 1)

let any_fibers_ready t = any_fibers_ready t (t.threads_num - 1)

let next_index t i =
  let i = i + 1 in
  if i < t.threads_num then i else 0

let[@inline never] wakeup_heartbeat t =
  Mutex.lock t.mutex;
  let state = t.state in
  if state_arrhytmia <= state then begin
    t.state <- state land lnot state_arrhytmia;
    Mutex.unlock t.mutex;
    Condition.broadcast t.heartbeat_condition
  end
  else begin
    Mutex.unlock t.mutex
  end

let[@inline] wakeup_heartbeat t =
  if state_arrhytmia <= t.state then wakeup_heartbeat t

let exec ready (Per_thread p : per_thread) t =
  p.remaining_quota <- t.quota;
  let fiber =
    match ready with
    | Spawn (fiber, _)
    | Return (fiber, _)
    | Continue (fiber, _)
    | Resume (fiber, _) ->
        fiber
  in
  p.fiber <- Fiber.Maybe.of_fiber fiber;
  match ready with
  | Spawn (_, main) -> Effect.Deep.match_with main fiber t.handler
  | Return (_, k) -> Effect.Deep.continue k ()
  | Continue (_, k) -> Fiber.continue fiber k ()
  | Resume (_, k) -> Fiber.resume fiber k

let rec next (Per_thread p as pt : per_thread) =
  let ready =
    let c = p.countdown_to_steal in
    if 0 < c then begin
      p.countdown_to_steal <- c - 1;
      p.ready
    end
    else begin
      let t = p.context in
      if 1 < t.threads_num then begin
        let i = Random.int t.threads_num in
        p.countdown_to_steal <- Mpmcq.length p.ready + 1;
        match Array.unsafe_get t.threads i with
        | Nothing -> p.ready
        | Per_thread v ->
            if p.countdown_to_steal <= Mpmcq.length v.ready then v.ready
            else p.ready
      end
      else begin
        p.countdown_to_steal <- 1_000;
        p.ready
      end
    end
  in
  match Mpmcq.pop_exn ready with
  | ready ->
      let t = p.context in
      exec ready pt t
  | exception Mpmcq.Empty ->
      p.fiber <- Fiber.Maybe.nothing;
      let t = p.context in
      try_steal pt t (next_index t p.index)

and try_steal (Per_thread p as pt : per_thread) t i =
  if p.index <> i then begin
    match Array.unsafe_get t.threads i with
    | Nothing -> try_steal pt t (next_index t i)
    | Per_thread other_p -> begin
        match Mpmcq.pop_exn other_p.ready with
        | ready -> exec ready pt t
        | exception Mpmcq.Empty -> try_steal pt t (next_index t i)
      end
  end
  else wait pt t

and wait (pt : per_thread) t =
  if any_fibers_alive t then begin
    Mutex.lock t.mutex;
    let state = t.state in
    if state != state lor state_idlers land lnot state_arrhytmia then
      t.state <- state lor state_idlers land lnot state_arrhytmia;
    if state_arrhytmia <= state then Condition.broadcast t.heartbeat_condition;
    if state < state_killed && not (any_fibers_ready t) then begin
      match Condition.wait t.worker_condition t.mutex with
      | () ->
          let state = t.state in
          if state != state lor state_idlers then
            t.state <- state lor state_idlers;
          Mutex.unlock t.mutex;
          if state < state_killed then next pt
      | exception async_exn ->
          let state = t.state in
          if state != state lor state_idlers then
            t.state <- state lor state_idlers;
          Mutex.unlock t.mutex;
          raise async_exn
    end
    else begin
      Mutex.unlock t.mutex;
      if state < state_killed then next pt
    end
  end
  else begin
    kill t
  end

let default_fatal_exn_handler exn =
  prerr_string "Fatal error: exception ";
  prerr_string (Printexc.to_string exn);
  prerr_char '\n';
  Printexc.print_backtrace stderr;
  flush stderr;
  exit 2

let per_thread context =
  let ready = Mpmcq.create ~padded:true () in
  let (Per_thread p as pt : per_thread) =
    Per_thread
      {
        ready;
        resume = Obj.magic ();
        return = None;
        discontinue = None;
        current = None;
        yield = None;
        context;
        index = 0;
        num_started = 0;
        num_stopped = 0;
        fiber = Fiber.Maybe.nothing;
        remaining_quota = 0;
        countdown_to_steal = 1;
      }
  in
  p.resume <-
    (fun trigger fiber k ->
      let resume = Resume (fiber, k) in
      let (Per_thread p_original) = (pt : per_thread) in
      let ready, signal =
        match Picos_thread.TLS.get_exn per_thread_key with
        | Per_thread p_current when p_original.context == p_current.context ->
            (* We are running on a thread of this scheduler *)
            (p_current.ready, false)
        | _ | (exception Picos_thread.TLS.Not_set) ->
            (* We are running on a foreign thread *)
            (p_original.ready, true)
      in
      if Fiber.unsuspend fiber trigger then Mpmcq.push ready resume
      else Mpmcq.push_head ready resume;
      let t = p_original.context in
      wakeup_heartbeat t;
      if signal then Condition.signal t.worker_condition);
  p.current <-
    Some
      (fun k ->
        let (Per_thread p) = (pt : per_thread) in
        let fiber = Fiber.Maybe.to_fiber p.fiber in
        Effect.Deep.continue k fiber);
  p.yield <-
    Some
      (fun k ->
        let (Per_thread p as pt) = (pt : per_thread) in
        let fiber = Fiber.Maybe.to_fiber p.fiber in
        Mpmcq.push p.ready (Continue (fiber, k));
        next pt);
  p.return <-
    Some
      (fun k ->
        let (Per_thread p) = (pt : per_thread) in
        let remaining_quota = p.remaining_quota - 1 in
        if 0 < remaining_quota then begin
          p.remaining_quota <- remaining_quota;
          Effect.Deep.continue k ()
        end
        else begin
          Mpmcq.push p.ready (Return (Fiber.Maybe.to_fiber p.fiber, k));
          next pt
        end);
  p.discontinue <-
    Some
      (fun k ->
        let (Per_thread p) = (pt : per_thread) in
        let fiber = Fiber.Maybe.to_fiber p.fiber in
        Fiber.continue fiber k ());
  (pt : per_thread)

let[@inline never] returned value old_p =
  (* TODO: maybe remove from [t.threads]? *)
  Picos_thread.TLS.set per_thread_key old_p;
  value

let[@inline never] raised exn old_p =
  let bt = Printexc.get_raw_backtrace () in
  Picos_thread.TLS.set per_thread_key old_p;
  Printexc.raise_with_backtrace exn bt

let[@inline never] with_per_thread new_pt fn old_p =
  match fn (new_pt : per_thread) with
  | value -> returned value old_p
  | exception exn -> raised exn old_p

let rec heartbeat_thread t rounds =
  if state_idlers lor state_running = t.state && any_fibers_ready t then begin
    if Mutex.try_lock t.mutex then begin
      t.state <- t.state land lnot state_idlers;
      Mutex.unlock t.mutex;
      Condition.signal t.worker_condition
    end;
    Thread.yield ();
    heartbeat_thread t t.heartbeat_rounds
  end
  else begin
    if 0 < rounds then begin
      if t.state <= state_killed then begin
        Thread.delay t.heartbeat_delay;
        heartbeat_thread t (rounds - 1)
      end
    end
    else begin
      if Mutex.try_lock t.mutex then begin
        let state = t.state in
        if state < state_killed then begin
          t.state <- state lor state_arrhytmia;
          Condition.wait t.heartbeat_condition t.mutex
        end;
        Mutex.unlock t.mutex;
        heartbeat_thread t t.heartbeat_rounds
      end
      else heartbeat_thread t 0
    end
  end

let heartbeat_thread t =
  try heartbeat_thread t t.heartbeat_rounds
  with exn ->
    kill t;
    t.handler.exnc exn

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
      if t.threads_num = 0 then begin
        Atomic.incr t.num_started;
        let _ = Thread.create heartbeat_thread t in
        ()
      end
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
  with_per_thread new_pt fn old_p

let effc : type a. a Effect.t -> ((a, _) Effect.Deep.continuation -> _) option =
 fun e ->
  let (Per_thread p as pt) = get_per_thread () in
  match e with
  | Fiber.Current -> p.current
  | Fiber.Spawn r ->
      let fiber = Fiber.Maybe.to_fiber p.fiber in
      if Fiber.is_canceled fiber then p.discontinue
      else begin
        p.num_started <- p.num_started + 1;
        (* The queue [push] includes a full fence, which means the increment
           of [num_started] will happen before increment of [num_stopped]. *)
        Mpmcq.push p.ready (Spawn (r.fiber, r.main));
        wakeup_heartbeat p.context;
        p.return
      end
  | Fiber.Yield -> p.yield
  | Computation.Cancel_after r -> begin
      let fiber = Fiber.Maybe.to_fiber p.fiber in
      if Fiber.is_canceled fiber then p.discontinue
      else
        match
          Select.cancel_after r.computation ~seconds:r.seconds r.exn r.bt
        with
        | () -> p.return
        | exception exn ->
            let bt = Printexc.get_raw_backtrace () in
            Some (fun k -> Effect.Deep.discontinue_with_backtrace k exn bt)
    end
  | Trigger.Await trigger ->
      Some
        (fun k ->
          let fiber = Fiber.Maybe.to_fiber p.fiber in
          if Fiber.try_suspend fiber trigger fiber k p.resume then next pt
          else
            let remaining_quota = p.remaining_quota - 1 in
            if 0 < remaining_quota then begin
              p.remaining_quota <- remaining_quota;
              Fiber.resume fiber k
            end
            else begin
              Mpmcq.push p.ready (Resume (fiber, k));
              next pt
            end)
  | _ -> None

let retc () =
  let (Per_thread p as pt) = get_per_thread () in
  p.num_stopped <- p.num_stopped + 1;
  next pt

let context ?heartbeat_delay ?heartbeat_rounds ?quota ?fatal_exn_handler () =
  let heartbeat_delay =
    match heartbeat_delay with
    | None -> 0.005
    | Some delay ->
        if not (0.0 <= delay && delay <= 1.0) then
          heartbeat_delay_out_of_range ()
        else delay
  in
  let heartbeat_rounds =
    match heartbeat_rounds with
    | None -> 100
    | Some rounds -> if rounds < 0 then heartbeat_rounds_negative () else rounds
  in
  let quota =
    match quota with
    | None -> Int.max_int
    | Some quota -> if quota <= 0 then quota_non_positive quota else quota
  in
  let exnc =
    match fatal_exn_handler with
    | None -> default_fatal_exn_handler
    | Some handler ->
        fun exn ->
          let (Per_thread p) = get_per_thread () in
          kill p.context;
          handler exn;
          raise exn
  in
  Select.check_configured ();
  let mutex = Mutex.create ()
  and worker_condition = Condition.create ()
  and heartbeat_condition = Condition.create ()
  and num_started = Atomic.make 0 in
  {
    state = 0;
    num_started;
    mutex;
    worker_condition;
    heartbeat_condition;
    heartbeat_delay;
    heartbeat_rounds;
    handler = { retc; exnc; effc };
    quota;
    threads = Array.make 15 Nothing;
    threads_num = 0;
  }

let runner_on_this_thread t =
  Select.check_configured ();
  with_per_thread t next

let run_fiber ?context:t_opt fiber main =
  let t = match t_opt with None -> context () | Some t -> t in
  with_per_thread t @@ fun (Per_thread p) ->
  Mutex.lock t.mutex;
  let state = t.state in
  if state = state lor state_running then begin
    Mutex.unlock t.mutex;
    already_running ()
  end
  else begin
    t.state <- state lor state_running;
    Mutex.unlock t.mutex;
    p.remaining_quota <- t.quota;
    p.fiber <- Fiber.Maybe.of_fiber fiber;
    Effect.Deep.match_with main fiber t.handler
  end

let[@inline never] run ?context fiber main computation =
  run_fiber ?context fiber main;
  Computation.peek_exn computation

let run ?context ?forbid main =
  let forbid = match forbid with None -> false | Some forbid -> forbid in
  let computation = Computation.create ~mode:`LIFO () in
  let fiber = Fiber.create ~forbid computation in
  let main _ = Computation.capture computation main () in
  run ?context fiber main computation

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
        begin
          match Domain.join runner with
          | None -> ()
          | Some (exn, bt) -> Printexc.raise_with_backtrace exn bt
        end;
        result
    | exception exn ->
        let bt = Printexc.get_raw_backtrace () in
        begin
          match Domain.join runner with
          | None -> ()
          | Some (exn, bt) -> Printexc.raise_with_backtrace exn bt
        end;
        Printexc.raise_with_backtrace exn bt

let run_fiber_on ?heartbeat_delay ?heartbeat_rounds ?quota ?fatal_exn_handler
    ~n_domains fiber main =
  if n_domains < 1 then invalid_arg "n_domains must be positive";
  let context =
    context ?heartbeat_delay ?heartbeat_rounds ?quota ?fatal_exn_handler ()
  in
  let runner_main =
    if n_domains = 1 then fun () -> None
    else
      let bt_status = Printexc.backtrace_status () in
      fun () ->
        Printexc.record_backtrace bt_status;
        match runner_on_this_thread context with
        | () -> None
        | exception exn ->
            let bt = Printexc.get_raw_backtrace () in
            Some (exn, bt)
  in
  run_fiber_on n_domains fiber main runner_main context

let[@inline never] run_on ?heartbeat_delay ?heartbeat_rounds ?quota
    ?fatal_exn_handler ~n_domains fiber main computation =
  run_fiber_on ?heartbeat_delay ?heartbeat_rounds ?quota ?fatal_exn_handler
    ~n_domains fiber main;
  Computation.peek_exn computation

let run_on ?heartbeat_delay ?heartbeat_rounds ?quota ?fatal_exn_handler
    ~n_domains ?forbid main =
  let forbid = match forbid with None -> false | Some forbid -> forbid in
  let computation = Computation.create ~mode:`LIFO () in
  let fiber = Fiber.create ~forbid computation in
  let main _ = Computation.capture computation main () in
  run_on ?heartbeat_delay ?heartbeat_rounds ?quota ?fatal_exn_handler ~n_domains
    fiber main computation
