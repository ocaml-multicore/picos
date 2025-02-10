open Picos_std_awaitable

type t = int Awaitable.t

let parties_bits = (Sys.int_size - 2) / 2
let max_parties = (1 lsl parties_bits) - 1
let parties_mask = max_parties
let sense_bit = 1 lsl parties_bits
let awaiting_shift = 1 + parties_bits
let awaiting_one = 1 lsl awaiting_shift
let poisoned_bit = Int.min_int

let create ?padded parties =
  if parties <= 0 || max_parties < parties then
    invalid_arg "invalid number of parties";
  Awaitable.make ?padded (parties lor (parties lsl awaiting_shift))

exception Poisoned

let await t =
  let before = Awaitable.fetch_and_add t (-awaiting_one) - awaiting_one in

  let finish =
    let parties = before land parties_mask in
    let sense_bit = lnot before land sense_bit in
    parties lor sense_bit lor (parties lsl awaiting_shift)
  in

  if before < awaiting_one then
    if Awaitable.compare_and_set t (before land lnot poisoned_bit) finish then
      Awaitable.broadcast t
    else
      let _ : int = Awaitable.fetch_and_add t awaiting_one in
      raise Poisoned
  else
    let state = ref before in
    while 0 < !state && !state != finish do
      Awaitable.await t !state;
      state := Awaitable.get t
    done;
    if !state < 0 then raise Poisoned

let rec poison t =
  let before = Awaitable.get t in
  if 0 < before then
    let after =
      let parties = before land parties_mask in
      let sense_bit = before land sense_bit in
      parties lor sense_bit lor (parties lsl awaiting_shift) lor poisoned_bit
    in
    if Awaitable.compare_and_set t before after then Awaitable.broadcast t
    else poison t
