let discontinue k exn =
  Effect.Deep.discontinue_with_backtrace k exn (Printexc.get_raw_backtrace ())

let using (h : _ t) c =
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
