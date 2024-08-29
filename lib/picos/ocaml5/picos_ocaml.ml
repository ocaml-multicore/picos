open Picos_bootstrap

module Trigger = struct
  type _ Effect.t +=
    | Await : Trigger.t -> (exn * Printexc.raw_backtrace) option Effect.t

  let await t = if Trigger.is_initial t then Effect.perform (Await t) else None
end

module Fiber = struct
  let resume t k = Effect.Deep.continue k (Fiber.canceled t)
  let resume_with t k h = Effect.Shallow.continue_with k (Fiber.canceled t) h

  let continue t k v =
    match Fiber.canceled t with
    | None -> Effect.Deep.continue k v
    | Some (exn, bt) -> Effect.Deep.discontinue_with_backtrace k exn bt

  let continue_with t k v h =
    match Fiber.canceled t with
    | None -> Effect.Shallow.continue_with k v h
    | Some (exn, bt) -> Effect.Shallow.discontinue_with_backtrace k exn bt h

  type _ Effect.t += Current : Fiber.t Effect.t

  let current () = Effect.perform Current

  type _ Effect.t +=
    | Spawn : { fiber : Fiber.t; main : Fiber.t -> unit } -> unit Effect.t

  let spawn fiber main = Effect.perform @@ Spawn { fiber; main }

  type _ Effect.t += Yield : unit Effect.t

  let yield () = Effect.perform Yield
end

module Computation = struct
  type _ Effect.t +=
    | Cancel_after : {
        seconds : float;
        exn : exn;
        bt : Printexc.raw_backtrace;
        computation : 'a Computation.t;
      }
        -> unit Effect.t

  let cancel_after computation ~seconds exn bt =
    Computation.check_non_negative seconds;
    Effect.perform (Cancel_after { seconds; exn; bt; computation })
end

module Handler = struct
  let discontinue k exn =
    Effect.Deep.discontinue_with_backtrace k exn (Printexc.get_raw_backtrace ())

  let using (h : _ Handler.t) c =
    let current =
      Some
        (fun k ->
          match h.current c with
          | fiber -> Effect.Deep.continue k fiber
          | exception exn -> discontinue k exn)
    and yield =
      Some
        (fun k ->
          match h.yield c with
          | () -> Effect.Deep.continue k ()
          | exception exn -> discontinue k exn)
    in
    let effc (type a) :
        a Effect.t -> ((a, _) Effect.Deep.continuation -> _) option = function
      | Fiber.Current -> current
      | Fiber.Spawn r ->
          Some
            (fun k ->
              match h.spawn c r.fiber r.main with
              | unit -> Effect.Deep.continue k unit
              | exception exn -> discontinue k exn)
      | Fiber.Yield -> yield
      | Trigger.Await trigger ->
          Some (fun k -> Effect.Deep.continue k (h.await c trigger))
      | Computation.Cancel_after r ->
          Some
            (fun k ->
              match
                h.cancel_after c r.computation ~seconds:r.seconds r.exn r.bt
              with
              | unit -> Effect.Deep.continue k unit
              | exception exn -> discontinue k exn)
      | _ -> None
    in
    let handler = Effect.Deep.{ retc = Fun.id; exnc = raise; effc } in
    fun main -> Effect.Deep.match_with main (h.current c) handler
end
