open Picos

type 'a t = 'a Computation.t

let of_value = Computation.returned
let await = Computation.await
let is_running = Computation.is_running

let try_terminate ?callstack t =
  let terminate_bt = Control.terminate_bt ?callstack () in
  Computation.try_cancel t terminate_bt

let terminate ?callstack t = try_terminate ?callstack t |> ignore
