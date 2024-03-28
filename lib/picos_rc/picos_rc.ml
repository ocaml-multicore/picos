type 'a t = { count : int Atomic.t; value : 'a; dispose : 'a -> unit }

(** The lowest bit indicates whether or not {!dispose} has been called. *)
let alive_bit = 0b01

(** The reference count start at the second lowest bit. *)
let one = 0b10

let make ~dispose value =
  { count = Atomic.make (one + alive_bit); value; dispose }

let unsafe_get t =
  if Atomic.get t.count <= alive_bit then
    invalid_arg "Picos_rc: resource already disposed";
  t.value

let rec dispose t backoff =
  let count = Atomic.get t.count in
  if one <= count && count land alive_bit <> 0 then
    if Atomic.compare_and_set t.count count (count - one - alive_bit) then begin
      if count <= alive_bit then t.dispose t.value
    end
    else dispose t (Backoff.once backoff)

let dispose t = dispose t Backoff.default

let has_been_disposed t =
  let count = Atomic.get t.count in
  count land alive_bit = 0

let rec decr t backoff =
  let count = Atomic.get t.count in
  if one <= count then
    if Atomic.compare_and_set t.count count (count - one) then begin
      if count <= alive_bit then t.dispose t.value
    end
    else decr t (Backoff.once backoff)
  else invalid_arg "Picos_rc: too many decr calls"

let decr t = decr t Backoff.default

let rec try_incr t backoff =
  let before = Atomic.get t.count in
  let after = before + one in
  (* The condition [one < after] also guards against overflow. *)
  one < after
  && (Atomic.compare_and_set t.count before after
     || try_incr t (Backoff.once backoff))

let try_incr t = try_incr t Backoff.default
