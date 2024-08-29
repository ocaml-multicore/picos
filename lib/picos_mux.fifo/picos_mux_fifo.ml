open Picos

let[@inline never] quota_non_positive () = invalid_arg "quota must be positive"

(* As a minor optimization, we avoid allocating closures, which take slightly
   more memory than values of this type. *)
type ready =
  | Spawn of Fiber.t * (Fiber.t -> unit)
  | Continue of Fiber.t * (unit, unit) Effect.Deep.continuation
  | Resume of
      Fiber.t
      * ((exn * Printexc.raw_backtrace) option, unit) Effect.Deep.continuation
  | Return of Fiber.Maybe.t * (unit, unit) Effect.Deep.continuation

module Mpscq = Picos_aux_mpscq

type t = {
  ready : ready Mpscq.t;
  needs_wakeup : bool Atomic.t;
  num_alive_fibers : int Atomic.t;
  mutex : Mutex.t;
  condition : Condition.t;
  resume :
    Trigger.t ->
    Fiber.t ->
    ((exn * Printexc.raw_backtrace) option, unit) Effect.Deep.continuation ->
    unit;
  current : ((Fiber.t, unit) Effect.Deep.continuation -> unit) option;
  yield : ((unit, unit) Effect.Deep.continuation -> unit) option;
  return : ((unit, unit) Effect.Deep.continuation -> unit) option;
  discontinue : ((unit, unit) Effect.Deep.continuation -> unit) option;
  handler : (unit, unit) Effect.Deep.handler;
  quota : int;
  mutable fiber : Fiber.Maybe.t;
  mutable remaining_quota : int;
}

let rec next t =
  match Mpscq.pop_exn t.ready with
  | Spawn (fiber, main) ->
      t.fiber <- Fiber.Maybe.of_fiber fiber;
      t.remaining_quota <- t.quota;
      Effect.Deep.match_with main fiber t.handler
  | Return (fiber, k) ->
      t.fiber <- fiber;
      t.remaining_quota <- t.quota;
      Effect.Deep.continue k ()
  | Continue (fiber, k) ->
      t.fiber <- Fiber.Maybe.of_fiber fiber;
      t.remaining_quota <- t.quota;
      Fiber.continue fiber k ()
  | Resume (fiber, k) ->
      t.fiber <- Fiber.Maybe.of_fiber fiber;
      t.remaining_quota <- t.quota;
      Fiber.resume fiber k
  | exception Mpscq.Empty ->
      t.fiber <- Fiber.Maybe.nothing;
      if Atomic.get t.num_alive_fibers <> 0 then begin
        if Atomic.get t.needs_wakeup then begin
          Mutex.lock t.mutex;
          match
            if Atomic.get t.needs_wakeup then
              (* We assume that there is no poll point after the above
                 [Mutex.lock] and before the below [Condition.wait] is ready to
                 be woken up by a [Condition.broadcast]. *)
              Condition.wait t.condition t.mutex
          with
          | () -> Mutex.unlock t.mutex
          | exception exn ->
              Mutex.unlock t.mutex;
              raise exn
        end
        else Atomic.set t.needs_wakeup true;
        next t
      end

let run_fiber ?quota ?fatal_exn_handler:(exnc : _ = raise) fiber main =
  let quota =
    match quota with
    | None -> Int.max_int
    | Some quota ->
        if quota <= 0 then quota_non_positive ();
        quota
  in
  Select.check_configured ();
  let ready = Mpscq.create ~padded:true ()
  and needs_wakeup = Atomic.make false |> Multicore_magic.copy_as_padded
  and num_alive_fibers = Atomic.make 1 |> Multicore_magic.copy_as_padded
  and mutex = Mutex.create ()
  and condition = Condition.create () in
  let rec t =
    {
      ready;
      fiber = Fiber.Maybe.nothing;
      needs_wakeup;
      num_alive_fibers;
      mutex;
      condition;
      resume;
      current;
      yield;
      return;
      discontinue;
      handler;
      quota;
      remaining_quota = quota;
    }
  and current =
    (* The current handler must never propagate cancelation, but it would be
       possible to continue some other fiber and resume the current fiber
       later. *)
    Some
      (fun k ->
        let fiber = Fiber.Maybe.to_fiber t.fiber in
        Effect.Deep.continue k fiber)
  and yield =
    Some
      (fun k ->
        let fiber = Fiber.Maybe.to_fiber t.fiber in
        Mpscq.push t.ready (Continue (fiber, k));
        next t)
  and return =
    Some
      (fun k ->
        let remaining_quota = t.remaining_quota - 1 in
        if 0 < remaining_quota then begin
          t.remaining_quota <- remaining_quota;
          Effect.Deep.continue k ()
        end
        else begin
          Mpscq.push t.ready (Return (t.fiber, k));
          next t
        end)
  and discontinue =
    Some
      (fun k ->
        let fiber = Fiber.Maybe.to_fiber t.fiber in
        Fiber.continue fiber k ())
  and handler = { retc; exnc; effc }
  and[@alert "-handler"] effc :
      type a. a Effect.t -> ((a, _) Effect.Deep.continuation -> _) option =
    function
    | Fiber.Current ->
        (* We handle [Current] first as it is perhaps the most latency
           sensitive effect. *)
        t.current
    | Fiber.Spawn r ->
        (* We check cancelation status once and then either perform the
           whole operation or discontinue the fiber. *)
        let fiber = Fiber.Maybe.to_fiber t.fiber in
        if Fiber.is_canceled fiber then t.discontinue
        else begin
          Atomic.incr t.num_alive_fibers;
          Mpscq.push t.ready (Spawn (r.fiber, r.main));
          t.return
        end
    | Fiber.Yield -> t.yield
    | Computation.Cancel_after r -> begin
        (* We check cancelation status once and then either perform the
           whole operation or discontinue the fiber. *)
        let fiber = Fiber.Maybe.to_fiber t.fiber in
        if Fiber.is_canceled fiber then t.discontinue
        else
          match
            Select.cancel_after r.computation ~seconds:r.seconds r.exn r.bt
          with
          | () -> t.return
          | exception exn ->
              let bt = Printexc.get_raw_backtrace () in
              Some (fun k -> Effect.Deep.discontinue_with_backtrace k exn bt)
      end
    | Trigger.Await trigger ->
        Some
          (fun k ->
            let fiber = Fiber.Maybe.to_fiber t.fiber in
            if Fiber.try_suspend fiber trigger fiber k t.resume then next t
            else
              let remaining_quota = t.remaining_quota - 1 in
              if 0 < remaining_quota then begin
                t.remaining_quota <- remaining_quota;
                Fiber.resume fiber k
              end
              else begin
                Mpscq.push t.ready (Resume (fiber, k));
                next t
              end)
    | _ -> None
  and retc () =
    Atomic.decr t.num_alive_fibers;
    next t
  and resume trigger fiber k =
    let resume = Resume (fiber, k) in
    if Fiber.unsuspend fiber trigger then
      (* The fiber has not been canceled, so we queue the fiber normally. *)
      Mpscq.push t.ready resume
    else
      (* The fiber has been canceled, so we give priority to it in this
         scheduler. *)
      Mpscq.push_head t.ready resume;
    (* As the trigger might have been signaled from another domain or systhread
       outside of the scheduler, we check whether the scheduler needs to be
       woken up and take care of it if necessary. *)
    if
      Atomic.get t.needs_wakeup
      && Atomic.compare_and_set t.needs_wakeup true false
    then begin
      begin
        match Mutex.lock t.mutex with
        | () -> Mutex.unlock t.mutex
        | exception Sys_error _ ->
            (* This should mean that [resume] was called from a signal handler
               running on the scheduler thread.  If the assumption about not
               having poll points holds, the [Condition.broadcast] should now be
               able to wake up the [Condition.wait] in the scheduler. *)
            ()
      end;
      Condition.broadcast t.condition
    end
  in
  Mpscq.push t.ready (Spawn (fiber, main));
  next t

let run ?quota ?fatal_exn_handler ?(forbid = false) main =
  let computation = Computation.create ~mode:`LIFO () in
  let fiber = Fiber.create ~forbid computation in
  let main _ = Computation.capture computation main () in
  run_fiber ?quota ?fatal_exn_handler fiber main;
  Computation.await computation
