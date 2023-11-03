open Picos
open Foundation

type t = {
  ready : (unit -> unit) Mpsc_queue.t;
  needs_wakeup : bool Atomic.t;
  num_alive_fibers : int ref;
  mc : Mutex_and_condition.t;
}

let rec next t =
  match Mpsc_queue.dequeue t.ready with
  | work -> work ()
  | exception Mpsc_queue.Empty ->
      if !(t.num_alive_fibers) <> 0 then begin
        if Atomic.get t.needs_wakeup then begin
          Mutex_and_condition.lock t.mc;
          match
            if Atomic.get t.needs_wakeup then Mutex_and_condition.wait t.mc
          with
          | () -> Mutex_and_condition.unlock t.mc
          | exception exn ->
              Mutex_and_condition.unlock t.mc;
              raise exn
        end
        else Atomic.set t.needs_wakeup true;
        next t
      end

let run ~forbid main =
  let ready = Mpsc_queue.create () in
  let needs_wakeup = Atomic.make false in
  let num_alive_fibers = ref 1 in
  let mc = Mutex_and_condition.get () in
  let t = { ready; needs_wakeup; num_alive_fibers; mc } in
  let resume trigger fiber k =
    if not (Fiber.has_forbidden fiber) then Fiber.detach fiber trigger;
    let work () = Effect.Deep.continue k (Fiber.canceled fiber) in
    Mpsc_queue.enqueue t.ready work;
    if
      Atomic.get t.needs_wakeup
      && Atomic.compare_and_set t.needs_wakeup true false
    then Mutex_and_condition.broadcast t.mc
  in
  let rec fork fiber main =
    let current = Some (fun k -> Fiber.continue fiber k fiber)
    and yield =
      Some
        (fun k ->
          Mpsc_queue.enqueue t.ready (Fiber.continue fiber k);
          next t)
    in
    let effc (type a) :
        a Effect.t -> ((a, _) Effect.Deep.continuation -> _) option = function
      | Fiber.Current ->
          (* We handle [Current] first as it is perhaps the most latency
             sensitive effect. *)
          current
      | Fiber.Spawn { forbid; computation; mains } ->
          Some
            (fun k ->
              match Fiber.canceled fiber with
              | None ->
                  mains
                  |> List.iter (fun main ->
                         let fiber = Fiber.create ~forbid computation in
                         Mpsc_queue.enqueue t.ready (fun () -> fork fiber main);
                         incr t.num_alive_fibers);
                  Effect.Deep.continue k ()
              | Some exn_bt -> Exn_bt.discontinue k exn_bt)
      | Fiber.Yield -> yield
      | Trigger.Await trigger ->
          (* We handle [Await] last as it is probably the least latency
             sensitive effect. *)
          Some
            (fun k ->
              if Fiber.has_forbidden fiber then begin
                (* Fiber has forbidden propagation of cancelation.  This is the
                   easy case to handle. *)
                if Trigger.on_signal trigger fiber k resume then begin
                  (* Fiber is now suspended and can be resumed through the
                     trigger.  We just continue the next ready fiber. *)
                  next t
                end
                else begin
                  (* The trigger was already signaled.  We could now freely
                     choose which fiber to continue here, but in this scheduler
                     we choose to continue the current fiber. *)
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
                  if Trigger.on_signal trigger fiber k resume then begin
                    (* Fiber is now suspended and can be resumed through the
                       trigger.  That can now happen by signaling the trigger
                       directly or by canceling the computation of the fiber,
                       which will also signal the trigger.  We just continue the
                       next ready fiber. *)
                    next t
                  end
                  else begin
                    (* The trigger was already signaled.  We first need to
                       ensure that the trigger is detached from the computation
                       of the fiber. *)
                    Fiber.detach fiber trigger;
                    (* We could now freely decide which fiber to continue, but
                       in this scheduler we choose to continue the current
                       fiber. *)
                    Effect.Deep.continue k (Fiber.canceled fiber)
                  end
                end
                else begin
                  (* We could not attach the trigger to the computation of the
                     fiber, which means that either the computation has been
                     canceled or the trigger has been signaled.  We still need
                     to ensure that the trigger really is signaled. *)
                  Trigger.signal trigger;
                  (* We could now freely decide which fiber to continue, but in
                     this scheduler we choose to continue the current fiber. *)
                  Effect.Deep.continue k (Fiber.canceled fiber)
                end
              end)
      | _ -> None
    and retc () =
      decr t.num_alive_fibers;
      next t
    in
    Effect.Deep.match_with main () { retc; exnc = raise; effc }
  in
  let computation = Computation.create () in
  fork (Fiber.create ~forbid computation) (Computation.capture computation main);
  Computation.await computation
