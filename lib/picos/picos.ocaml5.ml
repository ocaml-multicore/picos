module TLS = TLS

module DLS = struct
  include Domain.DLS

  let new_key compute = new_key compute
end

module Exn_bt = struct
  include Exn_bt

  let discontinue k { exn; bt } =
    Effect.Deep.discontinue_with_backtrace k exn bt

  let discontinue_with k { exn; bt } handler =
    Effect.Shallow.discontinue_with_backtrace k exn bt handler
end

module Trigger = struct
  include Bootstrap.Trigger

  type _ Effect.t += Await : [ `On | `Signal ] t -> Exn_bt.t option Effect.t

  let await t =
    match Atomic.get t with
    | Initial -> begin
        try Effect.perform (Await t)
        with Effect.Unhandled (Await t) -> Default.await t
      end
    | Signaled -> None
    | Awaiting _ -> awaiting ()
end

module Fiber = struct
  include Bootstrap.Fiber

  let continue (Fiber r) k v =
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

  type _ Effect.t += Current : [ `Sync | `Async ] t Effect.t

  let current () =
    try Effect.perform Current
    with Effect.Unhandled Current -> Default.current ()

  type _ Effect.t +=
    | Spawn : {
        forbid : bool;
        computation : 'a Bootstrap.Computation.as_cancelable;
        mains : (unit -> unit) list;
      }
        -> unit Effect.t

  let spawn ~forbid computation mains =
    try Effect.perform @@ Spawn { forbid; computation :> _; mains }
    with Effect.Unhandled (Spawn { forbid; computation; mains }) ->
      let _ = current () in
      Default.spawn forbid computation mains

  type _ Effect.t += Yield : unit Effect.t

  let yield () =
    try Effect.perform Yield
    with Effect.Unhandled Yield ->
      let _ = current () in
      Default.yield ()
end

module Computation = struct
  include Bootstrap.Computation

  let rec await t =
    match Atomic.get t with
    | Returned value -> value
    | Canceled exn_bt -> Exn_bt.raise exn_bt
    | Continue _ ->
        let trigger = Trigger.create () in
        if try_attach t trigger then begin
          match Trigger.await trigger with
          | None -> await t
          | Some exn_bt ->
              detach t trigger;
              Exn_bt.raise exn_bt
        end
        else await t

  type _ Effect.t +=
    | Cancel_after : {
        seconds : float;
        exn_bt : Exn_bt.t;
        computation : 'a as_cancelable;
      }
        -> unit Effect.t

  let cancel_after computation ~seconds exn_bt =
    if seconds < 0.0 then invalid_arg "Computation: negative seconds"
    else
      try Effect.perform (Cancel_after { seconds; exn_bt; computation })
      with Effect.Unhandled (Cancel_after { seconds; exn_bt; computation }) ->
        let _ = Fiber.current () in
        Default.cancel_after seconds exn_bt computation
end
