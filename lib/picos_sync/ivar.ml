open Picos

type 'a t = 'a Computation.t

let create () = Computation.create ()
let of_value = Computation.returned
let try_fill = Computation.try_return
let fill = Computation.return
let try_poison = Computation.try_cancel
let poison = Computation.cancel

let peek_opt ivar =
  if Computation.is_running ivar then None else Some (Computation.await ivar)

let read = Computation.await
let read_evt = Event.from_computation
