open Picos_bootstrap

module Trigger = struct
  type _ Effect.t += Await : Trigger.t -> Exn_bt.t option Effect.t

  let await t = if Trigger.is_initial t then Effect.perform (Await t) else None
end

module Fiber = struct
  let continue t k v =
    match Fiber.canceled t with
    | None -> Effect.Deep.continue k v
    | Some exn_bt -> Exn_bt.discontinue k exn_bt

  let continue_with t k v h =
    match Fiber.canceled t with
    | None -> Effect.Shallow.continue_with k v h
    | Some exn_bt -> Exn_bt.discontinue_with k exn_bt h

  type _ Effect.t += Current : Fiber.t Effect.t

  let current () = Effect.perform Current

  type _ Effect.t +=
    | Spawn : {
        forbid : bool;
        computation : 'a Computation.t;
        mains : (unit -> unit) list;
      }
        -> unit Effect.t

  let spawn ~forbid computation mains =
    Effect.perform @@ Spawn { forbid; computation; mains }

  type _ Effect.t += Yield : unit Effect.t

  let yield () = Effect.perform Yield
end

module Computation = struct
  type _ Effect.t +=
    | Cancel_after : {
        seconds : float;
        exn_bt : Exn_bt.t;
        computation : 'a Computation.t;
      }
        -> unit Effect.t

  let cancel_after computation ~seconds exn_bt =
    Computation.check_non_negative seconds;
    Effect.perform (Cancel_after { seconds; exn_bt; computation })
end

let set_picos_implementation _ =
  raise
    (Sys_error "Picos.set_picos_implementation must be called only on OCaml 4")
