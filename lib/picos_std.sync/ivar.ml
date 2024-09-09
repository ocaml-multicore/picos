open Picos
open Picos_std_event

type 'a t = 'a Computation.t

let create () = Computation.create ()
let of_value = Computation.returned
let try_fill = Computation.try_return
let fill = Computation.return
let try_poison = Computation.try_cancel
let poison = Computation.cancel

let peek_opt ivar =
  match Computation.peek_exn ivar with
  | value -> Some value
  | exception Computation.Running -> None

let read = Computation.await
let read_evt = Event.from_computation
