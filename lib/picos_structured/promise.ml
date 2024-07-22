open Picos
open Picos_sync

type 'a t = 'a Computation.t

let of_value = Computation.returned
let await = Computation.await
let completed = Event.from_computation
let is_running = Computation.is_running

let try_terminate ?callstack t =
  Computation.try_cancel t (Control.terminate_bt ?callstack ())

let terminate ?callstack t = try_terminate ?callstack t |> ignore

let terminate_after ?callstack t ~seconds =
  Computation.cancel_after t ~seconds (Control.terminate_bt ?callstack ())
