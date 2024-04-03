open Picos

type t = { fiber : Fiber.t; mutex : Mutex.t; condition : Condition.t }

let create ~forbid computation =
  let fiber = Fiber.create ~forbid computation in
  let mutex = Mutex.create () in
  let condition = Condition.create () in
  { fiber; mutex; condition }

let block trigger t =
  (* We block fibers (or threads) on a per thread mutex and condition. *)
  Mutex.lock t.mutex;
  match
    while not (Trigger.is_signaled trigger) do
      Condition.wait t.condition t.mutex
    done
  with
  | () -> Mutex.unlock t.mutex
  | exception exn ->
      (* Condition.wait may be interrupted by asynchronous exceptions and we
         must make sure to unlock even in that case. *)
      Mutex.unlock t.mutex;
      raise exn

let resume trigger t _ =
  let _is_canceled : bool = Fiber.unsuspend t.fiber trigger in
  (* This will be called when the trigger is signaled.  We simply broadcast on
     the per thread condition variable. *)
  Mutex.lock t.mutex;
  Mutex.unlock t.mutex;
  Condition.broadcast t.condition

let[@alert "-handler"] rec await t trigger =
  if Fiber.try_suspend t.fiber trigger t t resume then block trigger t;
  Fiber.canceled t.fiber

and current t =
  (* The current handler must never propagate cancelation, but it would be
     possible to yield here to run some other fiber before resuming the current
     fiber. *)
  t.fiber

and yield t =
  (* In other handlers we need to account for cancelation. *)
  Fiber.check t.fiber;
  Systhreads.yield ()

and cancel_after : type a. _ -> a Computation.t -> _ =
 (* We need an explicit type signature to allow OCaml to generalize the tyoe as
    all of the handlers are in a single recursive definition. *)
 fun t computation ~seconds exn_bt ->
  Fiber.check t.fiber;
  Select.cancel_after computation ~seconds exn_bt

and spawn : type a. _ -> forbid:bool -> a Computation.t -> _ =
 fun t ~forbid computation mains ->
  Fiber.check t.fiber;
  mains
  |> List.iter @@ fun main ->
     Systhreads.create
       (fun () ->
         (* We need to (recursively) install the handler on each new thread
            that we create. *)
         Handler.using handler (create ~forbid computation) main)
       ()
     |> ignore

and handler = Handler.{ current; spawn; yield; cancel_after; await }

let run ~forbid main =
  Handler.using handler (create ~forbid (Computation.create ())) main
