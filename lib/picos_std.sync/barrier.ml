open Picos_std_awaitable

type t = int Awaitable.t

let sense_bit = 1
let parties_shift = 1
let parties_bits = (Sys.int_size - 2) / 2
let max_parties = (1 lsl parties_bits) - 1
let parties_mask = max_parties lsl parties_shift
let awaiting_shift = parties_shift + parties_bits
let awaiting_one = 1 lsl awaiting_shift
let poisoned_bit = Int.min_int

let create ?padded parties =
  if parties <= 0 || max_parties < parties then
    invalid_arg "invalid number of parties";
  Awaitable.make ?padded
    ((parties lsl parties_shift) lor (parties lsl awaiting_shift))

exception Poisoned

let rec poison t =
  let before = Awaitable.get t in
  if 0 < before then
    let after =
      let parties_shifted = before land parties_mask in
      let after_sense = lnot before land sense_bit in
      parties_shifted lor after_sense
      lor (max_parties lsl awaiting_shift)
      lor poisoned_bit
    in
    if Awaitable.compare_and_set t before after then Awaitable.broadcast t
    else poison t

let await t =
  let[@inline never] poison_and_raise t =
    poison t;
    raise Poisoned
  in
  let prior = Awaitable.fetch_and_add t (-awaiting_one) in
  if awaiting_one < prior then begin
    let before = prior - awaiting_one in
    let after_sense = prior land sense_bit lxor sense_bit in
    if before < awaiting_one then
      let after =
        let parties_shifted = before land parties_mask in
        parties_shifted lor after_sense
        lor (parties_shifted lsl (awaiting_shift - parties_shift))
      in
      if Awaitable.compare_and_set t before after then Awaitable.broadcast t
      else
        (* The barrier is being misused?  Poison the barrier. *)
        poison_and_raise t
    else
      let state = ref before in
      match
        while !state land sense_bit <> after_sense do
          Awaitable.await t !state;
          state := Awaitable.get t
        done
      with
      | () -> if 0 <= !state then () else raise Poisoned
      | exception exn ->
          let bt = Printexc.get_raw_backtrace () in
          poison t;
          Printexc.raise_with_backtrace exn bt
  end
  else begin
    (* The barrier was poisoned.  Undo the decrement. *)
    Awaitable.fetch_and_add t awaiting_one |> ignore;
    poison_and_raise t
  end

let parties t = (Awaitable.get t land parties_mask) lsr parties_shift
