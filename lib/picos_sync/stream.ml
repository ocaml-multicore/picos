open Picos

type 'a cursor = Cons of ('a * 'a cursor) Computation.t [@@unboxed]
type 'a t = 'a cursor Atomic.t

let create ?(padded = false) () =
  let t = Atomic.make (Cons (Computation.create ())) in
  if padded then Multicore_magic.copy_as_padded t else t

let advance t tail curr =
  if Atomic.get t == Cons tail then
    Atomic.compare_and_set t (Cons tail) curr |> ignore

let help t tail =
  let _, curr = Computation.await tail in
  advance t tail curr

let rec push t ((_, curr) as next) backoff =
  let (Cons tail) = Atomic.get t in
  if not (Computation.try_return tail next) then begin
    let backoff = Backoff.once backoff in
    help t tail;
    push t next backoff
  end
  else advance t tail curr

let push t value = push t (value, Cons (Computation.create ())) Backoff.default

let rec poison t exn_bt =
  let (Cons tail) = Atomic.get t in
  if not (Computation.try_cancel tail exn_bt) then begin
    help t tail;
    poison t exn_bt
  end

let tap = Atomic.get
let read (Cons at) = Computation.await at
let pushed (Cons at) = Event.from_computation at
