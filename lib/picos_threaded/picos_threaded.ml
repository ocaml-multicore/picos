open Picos

let block trigger ptmc =
  (* We block fibers (or threads) on a per thread mutex and condition. *)
  Picos_ptmc.lock ptmc;
  match
    while not (Trigger.is_signaled trigger) do
      Picos_ptmc.wait ptmc
    done
  with
  | () -> Picos_ptmc.unlock ptmc
  | exception exn ->
      (* Condition.wait may be interrupted by asynchronous exceptions and we
         must make sure to unlock even in that case. *)
      Picos_ptmc.unlock ptmc;
      raise exn

let resume trigger fiber ptmc =
  let _is_canceled : bool = Fiber.unsuspend fiber trigger in
  (* This will be called when the trigger is signaled.  We simply broadcast on
     the per thread condition variable. *)
  Picos_ptmc.broadcast ptmc

let[@alert "-handler"] rec await fiber trigger =
  let ptmc = Picos_ptmc.get () in
  if Fiber.try_suspend fiber trigger fiber ptmc resume then block trigger ptmc;
  Fiber.canceled fiber

and current fiber =
  (* The current handler must never propagate cancelation, but it would be
     possible to yield here to run some other fiber before resuming the current
     fiber. *)
  fiber

and yield fiber =
  (* In other handlers we need to account for cancelation. *)
  Fiber.check fiber;
  Systhreads.yield ()

and cancel_after : type a. _ -> a Computation.t -> _ =
 (* We need an explicit type signature to allow OCaml to generalize the tyoe as
    all of the handlers are in a single recursive definition. *)
 fun fiber computation ~seconds exn_bt ->
  Fiber.check fiber;
  Select.cancel_after computation ~seconds exn_bt

and spawn : type a. _ -> forbid:bool -> a Computation.t -> _ =
 fun fiber ~forbid computation mains ->
  Fiber.check fiber;
  mains
  |> List.iter @@ fun main ->
     Systhreads.create
       (fun () ->
         (* We need to (recursively) install the handler on each new thread
            that we create. *)
         Handler.using handler (Fiber.create ~forbid computation) main)
       ()
     |> ignore

and handler = Handler.{ current; spawn; yield; cancel_after; await }

let run ~forbid main =
  Handler.using handler (Fiber.create ~forbid (Computation.create ())) main
