open Picos
open Picos_std_event

type 'a cursor = Cons of ('a * 'a cursor) Computation.t [@@unboxed]
type 'a t = 'a cursor Atomic.t

let create ?padded () =
  Multicore_magic.copy_as ?padded
    (Atomic.make (Cons (Computation.create ~mode:`LIFO ())))

let[@inline] advance t tail curr =
  if Atomic.get t == Cons tail then
    Atomic.compare_and_set t (Cons tail) curr |> ignore

let[@inline] help t tail =
  let _, curr = Computation.peek_exn tail in
  advance t tail curr

let rec push t ((_, curr) as next) backoff =
  let (Cons tail) = Atomic.get t in
  if Computation.try_return tail next then advance t tail curr
  else
    let backoff = Backoff.once backoff in
    help t tail;
    push t next backoff

let push t value =
  let next = (value, Cons (Computation.create ~mode:`LIFO ())) in
  push t next Backoff.default

let rec poison t exn bt backoff =
  let (Cons tail) = Atomic.get t in
  if not (Computation.try_cancel tail exn bt) then begin
    let backoff = Backoff.once backoff in
    help t tail;
    poison t exn bt backoff
  end

let peek_opt (Cons at) =
  match Computation.peek_exn at with
  | value -> Some value
  | exception Computation.Running -> None

let poison t exn bt = poison t exn bt Backoff.default
let tap = Atomic.get
let read (Cons at) = Computation.await at
let read_evt (Cons at) = Event.from_computation at
