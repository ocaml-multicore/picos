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
          | unit -> Effect.Deep.continue k unit
          | exception exn -> discontinue k exn)
    in
    let effc (type a) :
        a Effect.t -> ((a, _) Effect.Deep.continuation -> _) option = function
      | Fiber.Current -> current
      | Fiber.Spawn r ->
          Some
            (fun k ->
              match h.spawn c ~forbid:r.forbid r.computation r.mains with
              | unit -> Effect.Deep.continue k unit
              | exception exn -> discontinue k exn)
      | Fiber.Yield -> yield
      | Trigger.Await trigger ->
          Some (fun k -> Effect.Deep.continue k (h.await c trigger))
      | Computation.Cancel_after r ->
          Some
            (fun k ->
              match
                h.cancel_after c r.computation ~seconds:r.seconds r.exn_bt
              with
              | unit -> Effect.Deep.continue k unit
              | exception exn -> discontinue k exn)
      | _ -> None
    in
    let handler = Effect.Deep.{ retc = Fun.id; exnc = raise; effc } in
    fun main -> Effect.Deep.match_with main () handler
end
