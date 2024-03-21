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

let release _ _ ptmc =
  (* This will be called when the trigger is signaled.  We simply broadcast on
     the per thread condition variable. *)
  Picos_ptmc.broadcast ptmc

let[@alert "-handler"] rec await fiber trigger =
  (* The non-blocking logic below for suspending a fiber with support for
     parallelism safe cancelation is somewhat intricate.  Hopefully the comments
     help to understand it. *)
  if Fiber.has_forbidden fiber then begin
    (* Fiber has forbidden propagation of cancelation.  This is the easy case to
       handle. *)
    let ptmc = Picos_ptmc.get () in
    (* We could also have stored the per thread mutex and condition in the
       context and avoid getting it here, but this is likely cheap enough at
       this point anyway and makes the context trivial. *)
    if Trigger.on_signal trigger () ptmc release then begin
      (* Fiber is now suspended and can be resumed through the trigger.  We
         block the thread on the per thread mutex and condition waiting for the
         trigger. *)
      block trigger ptmc
    end;
    (* We return to continue the fiber. *)
    None
  end
  else begin
    (* Fiber permits propagation of cancelation.  We support cancelation and so
       first try to attach the trigger to the computation of the fiber. *)
    if Fiber.try_attach fiber trigger then
      (* The trigger was successfully attached, which means the computation has
         not been canceled. *)
      let ptmc = Picos_ptmc.get () in
      if Trigger.on_signal trigger () ptmc release then begin
        (* Fiber is now suspended and can be resumed through the trigger.  That
           can now happen by signaling the trigger directly or by canceling the
           computation of the fiber, which will also signal the trigger.  We
           block the thread on the per thread mutex and condition waiting for
           the trigger. *)
        block trigger ptmc
      end
      else begin
        (* The trigger was already signaled.  We first need to ensure that the
           trigger is detached from the computation of the fiber. *)
        Fiber.detach fiber trigger
      end
    else begin
      (* We could not attach the trigger to the computation of the fiber, which
         means that either the computation has been canceled or the trigger has
         been signaled.  We still need to ensure that the trigger really is put
         into the signaled state before the fiber is continued. *)
      Trigger.dispose trigger
    end;
    (* We return to continue or discontinue the fiber. *)
    Fiber.canceled fiber
  end

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
