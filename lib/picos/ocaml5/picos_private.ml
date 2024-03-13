module Exn_bt = Picos_exn_bt

module Trigger = struct
  include Picos_private_bootstrap.Trigger

  type _ Effect.t += Await : t -> Exn_bt.t option Effect.t

  let await (t : t) =
    match Atomic.get t with
    | Initial -> Effect.perform (Await t)
    | Signaled -> None
    | Awaiting _ -> awaiting ()
end

module Fiber = struct
  include Picos_private_bootstrap.Fiber

  let continue (Fiber r : Picos_private_bootstrap.Fiber.t) k v =
    if r.forbid then Effect.Deep.continue k v
    else
      match Atomic.get r.computation with
      | Canceled exn_bt -> Exn_bt.discontinue k exn_bt
      | Continue _ | Returned _ -> Effect.Deep.continue k v

  let continue_with (Fiber r) k v h =
    if r.forbid then Effect.Shallow.continue_with k v h
    else
      match Atomic.get r.computation with
      | Canceled exn_bt -> Exn_bt.discontinue_with k exn_bt h
      | Continue _ | Returned _ -> Effect.Shallow.continue_with k v h

  type _ Effect.t += Current : t Effect.t

  let current () = Effect.perform Current

  type _ Effect.t +=
    | Spawn : {
        forbid : bool;
        computation : 'a Picos_private_bootstrap.Computation.t;
        mains : (unit -> unit) list;
      }
        -> unit Effect.t

  let spawn ~forbid computation mains =
    Effect.perform @@ Spawn { forbid; computation; mains }

  type _ Effect.t += Yield : unit Effect.t

  let yield () = Effect.perform Yield
end

module Computation = struct
  include Picos_private_bootstrap.Computation

  type _ Effect.t +=
    | Cancel_after : {
        seconds : float;
        exn_bt : Exn_bt.t;
        computation : 'a t;
      }
        -> unit Effect.t

  let cancel_after computation ~seconds exn_bt =
    if 0.0 <= seconds then
      Effect.perform (Cancel_after { seconds; exn_bt; computation })
    else negative ()
end
