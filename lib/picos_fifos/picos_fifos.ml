open Picos
module Queue = Picos_mpsc_queue

(* As a minor optimization, we avoid allocating closures, which take slightly
   more memory than values of this type. *)
type ready =
  | Spawn of Fiber.t * (unit -> unit)
  | Continue of Fiber.t * (unit, unit) Effect.Deep.continuation
  | Resume of Fiber.t * (Exn_bt.t option, unit) Effect.Deep.continuation
  | Resume_forbidden of (Exn_bt.t option, unit) Effect.Deep.continuation

type t = {
  ready : ready Queue.t;
  needs_wakeup : bool Atomic.t;
  num_alive_fibers : int Atomic.t;
  mc : Picos_ptmc.t;
      (** We must store this for [resume], which may come from any thread. *)
  resume :
    Trigger.t ->
    Fiber.t ->
    (Exn_bt.t option, unit) Effect.Deep.continuation ->
    unit;
}

let rec next t =
  match Queue.pop_exn t.ready with
  | Spawn (fiber, main) ->
      let current =
        Some
          (fun k ->
            (* The current handler must never propagate cancelation, but it
               would be possible to continue some other fiber and resume the
               current fiber later. *)
            Effect.Deep.continue k fiber)
      and yield =
        Some
          (fun k ->
            Queue.push t.ready (Continue (fiber, k));
            next t)
      in
      let[@alert "-handler"] effc (type a) :
          a Effect.t -> ((a, _) Effect.Deep.continuation -> _) option = function
        | Fiber.Current ->
            (* We handle [Current] first as it is perhaps the most latency
               sensitive effect. *)
            current
        | Fiber.Spawn r ->
            Some
              (fun k ->
                (* We check cancelation status once and then either perform the
                   whole operation or discontinue the fiber. *)
                match Fiber.canceled fiber with
                | None ->
                    r.mains
                    |> List.iter (fun main ->
                           let fiber =
                             Fiber.create ~forbid:r.forbid r.computation
                           in
                           Queue.push t.ready (Spawn (fiber, main));
                           Atomic.incr t.num_alive_fibers);
                    (* We intentionally do not check cancelation status
                       again. *)
                    Effect.Deep.continue k ()
                | Some exn_bt -> Exn_bt.discontinue k exn_bt)
        | Fiber.Yield -> yield
        | Computation.Cancel_after r ->
            Some
              (fun k ->
                (* We check cancelation status once and then either perform the
                   whole operation or discontinue the fiber. *)
                match Fiber.canceled fiber with
                | None ->
                    Picos_select.cancel_after r.computation ~seconds:r.seconds
                      r.exn_bt;
                    (* We intentionally do not check cancelation status
                       again. *)
                    Effect.Deep.continue k ()
                | Some exn_bt -> Exn_bt.discontinue k exn_bt)
        | Trigger.Await trigger ->
            (* We handle [Await] last as it is probably the least latency
               sensitive effect.  It could also be that another fiber running in
               parallel is just about to signal the trigger, so checking the
               trigger last gives a tiny bit of time for that to happen and
               potentially allows us to make better/different decisions here. *)
            Some
              (fun k ->
                (* The non-blocking logic below for suspending a fiber with
                   support for parallelism safe cancelation is somewhat
                   intricate.  Hopefully the comments help to understand it. *)
                if Fiber.has_forbidden fiber then begin
                  (* Fiber has forbidden propagation of cancelation.  This is
                     the easy case to handle. *)
                  if Trigger.on_signal trigger fiber k t.resume then begin
                    (* Fiber is now suspended and can be resumed through the
                       trigger.  We just continue the next ready fiber. *)
                    next t
                  end
                  else begin
                    (* The trigger was already signaled.  We could now freely
                       choose which fiber to continue here, but in this
                       scheduler we choose to continue the current fiber. *)
                    Effect.Deep.continue k None
                  end
                end
                else begin
                  (* Fiber permits propagation of cancelation.  We support
                     cancelation and so first try to attach the trigger to the
                     computation of the fiber. *)
                  if Fiber.try_attach fiber trigger then begin
                    (* The trigger was successfully attached, which means the
                       computation has not been canceled. *)
                    if Trigger.on_signal trigger fiber k t.resume then begin
                      (* Fiber is now suspended and can be resumed through the
                         trigger.  That can now happen by signaling the trigger
                         directly or by canceling the computation of the fiber,
                         which will also signal the trigger.  We just continue
                         the next ready fiber. *)
                      next t
                    end
                    else begin
                      (* The trigger was already signaled.  We first need to
                         ensure that the trigger is detached from the
                         computation of the fiber. *)
                      Fiber.detach fiber trigger;
                      (* We could now freely decide which fiber to continue, but
                         in this scheduler we choose to continue the current
                         fiber. *)
                      Fiber.resume fiber k
                    end
                  end
                  else begin
                    (* We could not attach the trigger to the computation of the
                       fiber, which means that either the computation has been
                       canceled or the trigger has been signaled.  We still need
                       to ensure that the trigger really is put into the
                       signaled state before the fiber is continued. *)
                    Trigger.dispose trigger;
                    (* We could now freely decide which fiber to continue, but
                       in this scheduler we choose to continue the current
                       fiber. *)
                    Fiber.resume fiber k
                  end
                end)
        | _ -> None
      and retc () =
        Atomic.decr t.num_alive_fibers;
        next t
      in
      Effect.Deep.match_with main () { retc; exnc = raise; effc }
  | Continue (fiber, k) -> Fiber.continue fiber k ()
  | Resume (fiber, k) -> Fiber.resume fiber k
  | Resume_forbidden k -> Effect.Deep.continue k None
  | exception Queue.Empty ->
      if Atomic.get t.num_alive_fibers <> 0 then begin
        if Atomic.get t.needs_wakeup then begin
          Picos_ptmc.lock t.mc;
          match if Atomic.get t.needs_wakeup then Picos_ptmc.wait t.mc with
          | () -> Picos_ptmc.unlock t.mc
          | exception exn ->
              Picos_ptmc.unlock t.mc;
              raise exn
        end
        else Atomic.set t.needs_wakeup true;
        next t
      end

let run ~forbid main =
  let ready = Queue.create ()
  and needs_wakeup = Atomic.make false
  and num_alive_fibers = Atomic.make 1
  and mc = Picos_ptmc.get () in
  let rec t = { ready; needs_wakeup; num_alive_fibers; mc; resume }
  and resume trigger fiber k =
    begin
      if Fiber.has_forbidden fiber then
        (* Fiber has forbidden propagation of cancelation.  This is the easy
           case. *)
        Queue.push t.ready (Resume_forbidden k)
      else
        let resume = Resume (fiber, k) in
        if Fiber.is_canceled fiber then begin
          (* The fiber has been canceled so we give priority to it in this
             scheduler.

             Assuming fibers are written to cooperate and perform cleanup
             promptly, this can be advantageous as it allows resources to be
             released more quickly.  However, malicious or buggy fibers could
             use this to prevent other fibers from running. *)
          Queue.push_head t.ready resume
        end
        else begin
          (* The fiber hasn't yet been canceled.

             As propagation of cancelation was not forbidden, and we have
             attached a trigger, we need to ensure that the trigger will not be
             leaked. *)
          Fiber.detach fiber trigger;
          Queue.push t.ready resume
        end
    end;
    (* As the trigger might have been signaled from another domain or systhread
       outside of the scheduler, we check whether the scheduler needs to be
       woken up and take care of it if necessary. *)
    if
      Atomic.get t.needs_wakeup
      && Atomic.compare_and_set t.needs_wakeup true false
    then Picos_ptmc.broadcast t.mc
  in
  let computation = Computation.create () in
  let fiber = Fiber.create ~forbid computation in
  let main = Computation.capture computation main in
  Queue.push t.ready (Spawn (fiber, main));
  next t;
  Computation.await computation
