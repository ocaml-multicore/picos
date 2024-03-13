let[@alert "-handler"] on_unhandled =
  let try_handle :
      type a. a Effect.t -> ((a, _) Effect.Deep.continuation -> _) option =
    function
    | Picos.Computation.Cancel_after r ->
        Some
          (fun k ->
            Picos_select.cancel_after r.computation ~seconds:r.seconds r.exn_bt;
            Effect.Deep.continue k ())
    | _ -> None
  in
  Schedulers.Try_handle.{ try_handle }

let run main = Schedulers.Fifos.run ~on_unhandled ~forbid:false main
