open Picos

type t = Fiber.t

let create = Fiber.create

let block trigger ptmc fiber =
  Picos_ptmc.lock ptmc;
  match
    while not (Trigger.is_signaled trigger) do
      Picos_ptmc.wait ptmc
    done
  with
  | () ->
      Picos_ptmc.unlock ptmc;
      Fiber.canceled fiber
  | exception exn ->
      (* Condition.wait may be interrupted by asynchronous exceptions and we
         must make sure to unlock even in that case. *)
      Picos_ptmc.unlock ptmc;
      raise exn

let release _ _ ptmc = Picos_ptmc.broadcast ptmc

let[@alert "-handler"] rec await fiber trigger =
  if Fiber.has_forbidden fiber then
    (* We could also have stored the per thread mutex and condition in the
       context and avoid getting it here, but this is likely cheap enough at
       this point anyway and makes the context trivial. *)
    let ptmc = Picos_ptmc.get () in
    if Trigger.on_signal trigger () ptmc release then block trigger ptmc fiber
    else None
  else if Fiber.try_attach fiber trigger then
    let ptmc = Picos_ptmc.get () in
    if Trigger.on_signal trigger () ptmc release then block trigger ptmc fiber
    else begin
      Fiber.detach fiber trigger;
      Fiber.canceled fiber
    end
  else begin
    Trigger.dispose trigger;
    Fiber.canceled fiber
  end

and cancel_after :
    type a. t -> a Computation.t -> seconds:float -> Exn_bt.t -> unit =
 fun fiber computation ~seconds exn_bt ->
  Fiber.check fiber;
  Select.cancel_after computation ~seconds exn_bt

and current fiber =
  Fiber.check fiber;
  fiber

and yield fiber =
  Fiber.check fiber;
  Systhreads.yield ()

and spawn :
    type a. t -> forbid:bool -> a Computation.t -> (unit -> unit) list -> unit =
 fun fiber ~forbid computation mains ->
  Fiber.check fiber;
  mains
  |> List.iter @@ fun main ->
     Systhreads.create
       (fun () -> Picos.Handler.using handler (create ~forbid computation) main)
       ()
     |> ignore

and handler = Handler.{ current; spawn; yield; cancel_after; await }
