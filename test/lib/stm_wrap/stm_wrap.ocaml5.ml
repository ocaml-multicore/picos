open Picos

let wrap_cmd_seq th =
  let fiber = Fiber.create ~forbid:false (Computation.create ()) in
  let current = Some (fun k -> Effect.Deep.continue k fiber) in
  let effc (type a) (e : a Effect.t) :
      ((a, _) Effect.Deep.continuation -> _) option =
    match e with
    | Fiber.Current -> current
    | Trigger.Await t ->
        Some
          (fun k ->
            while not (Trigger.is_signaled t) do
              Domain.cpu_relax ()
            done;
            Effect.Deep.continue k None)
    | _ -> None
  in
  Effect.Deep.match_with th () { effc; exnc = raise; retc = Fun.id }
