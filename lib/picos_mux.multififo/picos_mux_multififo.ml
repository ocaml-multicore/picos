open Picos

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
      ready : ready Mpmcq.t;
      mutable resume :
        Trigger.t ->
        Fiber.t ->
        ((exn * Printexc.raw_backtrace) option, unit) Effect.Deep.continuation ->
        unit;
      mutable return : ((unit, unit) Effect.Deep.continuation -> unit) option;
      mutable discontinue :
        ((unit, unit) Effect.Deep.continuation -> unit) option;
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

let per_thread_key = Picos_thread.TLS.create ()

let[@inline] get_per_thread () : per_thread =
  match Picos_thread.TLS.get_exn per_thread_key with
  | Nothing as any -> not_worker any
  | Per_thread _ as pt -> pt

let get_thread t i : per_thread =
  match Array.unsafe_get t.threads i with
  | Nothing as any -> not_worker any
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
  Mpmcq.length p.ready != 0 || any_fibers_ready t (i - 1)

let any_fibers_ready t = any_fibers_ready t (t.threads_num - 1)

let next_index t i =
  let i = i + 1 in
  if i < t.threads_num then i else 0

let[@inline] relaxed_wakeup t ~known_not_empty ready =
  if t.num_waiters_non_zero && (known_not_empty || Mpmcq.length ready != 0) then begin
    Mutex.lock t.mutex;
    Mutex.unlock t.mutex;
    Condition.signal t.condition
  end

let exec ready (Per_thread p : per_thread) t =
  p.remaining_quota <- t.quota;
  p.fiber <-
    (match ready with
    | Spawn (fiber, _)
    | Return (fiber, _)
    | Continue (fiber, _)
    | Resume (fiber, _) ->
        Fiber.Maybe.of_fiber fiber);
  match ready with
  | Spawn (fiber, main) -> Effect.Deep.match_with main fiber t.handler
  | Return (_, k) -> Effect.Deep.continue k ()
  | Continue (fiber, k) -> Fiber.continue fiber k ()
  | Resume (fiber, k) -> Fiber.resume fiber k

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
        let (Per_thread v) = get_thread t (Random.int t.threads_num) in
        p.countdown_to_steal <- Mpmcq.length p.ready + 1;
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
      relaxed_wakeup t ~known_not_empty:false p.ready;
      exec ready pt t
  | exception Mpmcq.Empty ->
      p.fiber <- Fiber.Maybe.nothing;
      let t = p.context in
      try_steal pt t (next_index t p.index)

and try_steal (Per_thread p as pt : per_thread) t i =
  if p.index <> i then begin
    let (Per_thread other_p) = get_thread t i in
    match Mpmcq.pop_exn other_p.ready with
    | ready ->
        relaxed_wakeup t ~known_not_empty:false other_p.ready;
        exec ready pt t
    | exception Mpmcq.Empty -> try_steal pt t (next_index t i)
  end
  else wait pt t

and wait (pt : per_thread) t =
  if any_fibers_alive t then begin
    Mutex.lock t.mutex;
    let n = !(t.num_waiters) + 1 in
    t.num_waiters := n;
    if n = 1 then t.num_waiters_non_zero <- true;
    if (not (any_fibers_ready t)) && any_fibers_alive t then begin
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
      let n = !(t.num_waiters) - 1 in
      t.num_waiters := n;
      if n = 0 then t.num_waiters_non_zero <- false;
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
  let ready = Mpmcq.create ~padded:true () in
  let (Per_thread p as pt : per_thread) =
    Per_thread
      {
        ready;
        resume = Obj.magic ();
        return = Obj.magic ();
        discontinue = Obj.magic ();
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
      match Picos_thread.TLS.get_exn per_thread_key with
      | Per_thread p_current when p_original.context == p_current.context ->
          (* We are running on a thread of this scheduler *)
          if Fiber.unsuspend fiber trigger then
            Mpmcq.push p_current.ready resume
          else Mpmcq.push_head p_current.ready resume;
          relaxed_wakeup p_current.context ~known_not_empty:true p_current.ready
      | _ | (exception Picos_thread.TLS.Not_set) ->
          (* We are running on a foreign thread *)
          if Fiber.unsuspend fiber trigger then
            Mpmcq.push p_original.ready resume
          else Mpmcq.push_head p_original.ready resume;
          let t = p_original.context in
          let non_zero =
            match Mutex.lock t.mutex with
            | () ->
                let non_zero = t.num_waiters_non_zero in
                Mutex.unlock t.mutex;
                non_zero
            | exception Sys_error _ -> false
          in
          if non_zero then Condition.signal t.condition);
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
  with_per_thread new_pt fn old_p

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
      Mpmcq.push p.ready (Continue (fiber, k));
      next pt)

let effc : type a. a Effect.t -> ((a, _) Effect.Deep.continuation -> _) option =
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
        Mpmcq.push p.ready (Spawn (r.fiber, r.main));
        let t = p.context in
        relaxed_wakeup t ~known_not_empty:true p.ready;
        p.return
      end
  | Fiber.Yield -> yield
  | Computation.Cancel_after r -> begin
      let (Per_thread p) = get_per_thread () in
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
              Mpmcq.push p.ready (Resume (fiber, k));
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
    | Some quota -> if quota <= 0 then quota_non_positive quota else quota
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
  {
    num_waiters_non_zero = false;
    num_waiters;
    num_started;
    mutex;
    condition;
    handler = { retc; exnc; effc };
    quota;
    run = false;
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
  if t.run then begin
    Mutex.unlock t.mutex;
    already_running ()
  end
  else begin
    t.run <- true;
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
        | exception exn ->
            let bt = Printexc.get_raw_backtrace () in
            Some (exn, bt)
  in
  run_fiber_on n_domains fiber main runner_main context

let[@inline never] run_on ?quota ?fatal_exn_handler ~n_domains fiber main
    computation =
  run_fiber_on ?quota ?fatal_exn_handler ~n_domains fiber main;
  Computation.peek_exn computation

let run_on ?quota ?fatal_exn_handler ~n_domains ?forbid main =
  let forbid = match forbid with None -> false | Some forbid -> forbid in
  let computation = Computation.create ~mode:`LIFO () in
  let fiber = Fiber.create ~forbid computation in
  let main _ = Computation.capture computation main () in
  run_on ?quota ?fatal_exn_handler ~n_domains fiber main computation
