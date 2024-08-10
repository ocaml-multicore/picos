open Picos_bootstrap

module Trigger = struct
  type _ Effect.t += Await : Trigger.t -> Exn_bt.t option Effect.t
end

module Fiber = struct
  let resume t k = Effect.Deep.continue k (Fiber.canceled t)
  let resume_with t k h = Effect.Shallow.continue_with k (Fiber.canceled t) h

  let continue t k v =
    match Fiber.canceled t with
    | None -> Effect.Deep.continue k v
    | Some exn_bt -> Exn_bt.discontinue k exn_bt

  let continue_with t k v h =
    match Fiber.canceled t with
    | None -> Effect.Shallow.continue_with k v h
    | Some exn_bt -> Exn_bt.discontinue_with k exn_bt h

  type _ Effect.t += Current : Fiber.t Effect.t

  type _ Effect.t +=
    | Spawn : { fiber : Fiber.t; main : Fiber.t -> unit } -> unit Effect.t

  type _ Effect.t += Yield : unit Effect.t
end

module Computation = struct
  type _ Effect.t +=
    | Cancel_after : {
        seconds : float;
        exn_bt : Exn_bt.t;
        computation : 'a Computation.t;
      }
        -> unit Effect.t
end

module Handler = struct
  let default =
    let current _ = Effect.perform Fiber.Current
    and spawn _ fiber main = Effect.perform @@ Fiber.Spawn { fiber; main }
    and yield _ = Effect.perform Fiber.Yield
    and cancel_after _ computation ~seconds exn_bt =
      Picos_bootstrap.Computation.check_non_negative seconds;
      Effect.perform (Computation.Cancel_after { seconds; exn_bt; computation })
    and await _ t =
      if Picos_bootstrap.Trigger.is_initial t then
        Effect.perform (Trigger.Await t)
      else None
    in
    Handler.Packed
      { context = (); handler = { current; spawn; yield; cancel_after; await } }
end
