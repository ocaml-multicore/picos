open Picos

let[@inline never] quota_non_positive _ = invalid_arg "quota must be positive"

type ready =
  | Spawn of Fiber.t * (Fiber.t -> unit)
  | Continue of Fiber.t * (unit, unit) Effect.Deep.continuation
  | Resume of
      Fiber.t
      * ((exn * Printexc.raw_backtrace) option, unit) Effect.Deep.continuation
  | Return of Fiber.t * (unit, unit) Effect.Deep.continuation

module Mpscq = Picos_aux_mpscq

type t = {
  ready : ready Mpscq.t;
  needs_wakeup : bool Atomic.t;
  mutex : Mutex.t;
  condition : Condition.t;
  mutable resume :
    Trigger.t ->
    Fiber.t ->
    ((exn * Printexc.raw_backtrace) option, unit) Effect.Deep.continuation ->
    unit;
  mutable current : ((Fiber.t, unit) Effect.Deep.continuation -> unit) option;
  mutable yield : ((unit, unit) Effect.Deep.continuation -> unit) option;
  mutable return : ((unit, unit) Effect.Deep.continuation -> unit) option;
  mutable discontinue : ((unit, unit) Effect.Deep.continuation -> unit) option;
  mutable handler : (unit, unit) Effect.Deep.handler;
  per_thread : Fiber.Per_thread.t;
  quota : int;
  mutable remaining_quota : int;
  mutable num_alive_fibers : int;
}

let rec next t =
  match Mpscq.pop_exn t.ready with
  | ready -> begin
      t.remaining_quota <- t.quota;
      t.per_thread.current <-
        (match ready with
        | Spawn (fiber, _)
        | Continue (fiber, _)
        | Resume (fiber, _)
        | Return (fiber, _) ->
            Fiber.Maybe.of_fiber fiber);
      match ready with
      | Spawn (fiber, main) -> Effect.Deep.match_with main fiber t.handler
      | Return (_, k) -> Effect.Deep.continue k ()
      | Continue (fiber, k) -> Fiber.continue fiber k ()
      | Resume (fiber, k) -> Fiber.resume fiber k
    end
  | exception Mpscq.Empty ->
      t.per_thread.current <- Fiber.Maybe.nothing;
      if t.num_alive_fibers <> 0 then begin
        if Atomic.get t.needs_wakeup then begin
          Mutex.lock t.mutex;
          match
            if Atomic.get t.needs_wakeup then Condition.wait t.condition t.mutex
          with
          | () -> Mutex.unlock t.mutex
          | exception exn ->
              Mutex.unlock t.mutex;
              raise exn
        end
        else Atomic.set t.needs_wakeup true;
        next t
      end

let run_fiber ?quota ?fatal_exn_handler fiber main =
  Select.check_configured ();
  let t =
    let quota =
      match quota with
      | None -> Int.max_int
      | Some quota -> if quota <= 0 then quota_non_positive quota else quota
    in
    let per_thread = Fiber.Per_thread.get () in
    per_thread.current <- Fiber.Maybe.of_fiber fiber;
    {
      ready = Mpscq.create ~padded:true ();
      needs_wakeup = Atomic.make false |> Multicore_magic.copy_as_padded;
      mutex = Mutex.create ();
      condition = Condition.create ();
      resume = Obj.magic ();
      current = Obj.magic ();
      yield = Obj.magic ();
      return = Obj.magic ();
      discontinue = Obj.magic ();
      handler = Obj.magic ();
      per_thread;
      quota;
      remaining_quota = quota;
      num_alive_fibers = 1;
    }
  in
  t.handler <-
    {
      exnc = (match fatal_exn_handler with None -> raise | Some exnc -> exnc);
      effc =
        (fun (type a) (e : a Effect.t) ->
          match e with
          | Fiber.Current ->
              (t.current : ((a, _) Effect.Deep.continuation -> _) option)
          | Fiber.Spawn r ->
              let fiber = Fiber.Maybe.to_fiber t.per_thread.current in
              if Fiber.is_canceled fiber then t.discontinue
              else begin
                t.num_alive_fibers <- t.num_alive_fibers + 1;
                Mpscq.push t.ready (Spawn (r.fiber, r.main));
                t.return
              end
          | Fiber.Yield -> t.yield
          | Computation.Cancel_after r -> begin
              let fiber = Fiber.Maybe.to_fiber t.per_thread.current in
              if Fiber.is_canceled fiber then t.discontinue
              else
                match
                  Select.cancel_after r.computation ~seconds:r.seconds r.exn
                    r.bt
                with
                | () -> t.return
                | exception exn ->
                    let bt = Printexc.get_raw_backtrace () in
                    Some
                      (fun k -> Effect.Deep.discontinue_with_backtrace k exn bt)
            end
          | Trigger.Await trigger ->
              Some
                (fun k ->
                  let fiber = Fiber.Maybe.to_fiber t.per_thread.current in
                  if Fiber.try_suspend fiber trigger fiber k t.resume then
                    next t
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
          | _ -> None);
      retc =
        (fun () ->
          t.num_alive_fibers <- t.num_alive_fibers - 1;
          next t);
    };
  t.resume <-
    (fun trigger fiber k ->
      let resume = Resume (fiber, k) in
      if Fiber.unsuspend fiber trigger then Mpscq.push t.ready resume
      else Mpscq.push_head t.ready resume;
      if
        Atomic.get t.needs_wakeup
        && Atomic.compare_and_set t.needs_wakeup true false
      then begin
        begin
          match Mutex.lock t.mutex with
          | () -> Mutex.unlock t.mutex
          | exception Sys_error _ -> ()
        end;
        Condition.broadcast t.condition
      end);
  t.current <-
    Some
      (fun k ->
        let fiber = Fiber.Maybe.to_fiber t.per_thread.current in
        Effect.Deep.continue k fiber);
  t.yield <-
    Some
      (fun k ->
        let fiber = Fiber.Maybe.to_fiber t.per_thread.current in
        Mpscq.push t.ready (Continue (fiber, k));
        next t);
  t.return <-
    Some
      (fun k ->
        let remaining_quota = t.remaining_quota - 1 in
        if 0 < remaining_quota then begin
          t.remaining_quota <- remaining_quota;
          Effect.Deep.continue k ()
        end
        else begin
          Mpscq.push t.ready
            (Return (Fiber.Maybe.to_fiber t.per_thread.current, k));
          next t
        end);
  t.discontinue <-
    Some
      (fun k ->
        let fiber = Fiber.Maybe.to_fiber t.per_thread.current in
        Fiber.continue fiber k ());
  Effect.Deep.match_with main fiber t.handler

let[@inline never] run ?quota ?fatal_exn_handler fiber main computation =
  run_fiber ?quota ?fatal_exn_handler fiber main;
  Computation.peek_exn computation

let run ?quota ?fatal_exn_handler ?forbid main =
  let forbid = match forbid with None -> false | Some forbid -> forbid in
  let computation = Computation.create ~mode:`LIFO () in
  let fiber = Fiber.create ~forbid computation in
  let main _ = Computation.capture computation main () in
  run ?quota ?fatal_exn_handler fiber main computation
