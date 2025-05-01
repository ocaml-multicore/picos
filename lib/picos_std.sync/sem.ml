open Picos_std_awaitable

exception Poisoned

type t = int Awaitable.t

let no_awaiters = 1 lsl 0
let one_shift = 1
let one = 1 lsl one_shift
let max_value = (1 lsl (Sys.int_size - 2)) asr one_shift
let poisoned = 1 lsl (Sys.int_size - 2)

let create ?padded n =
  if n < 0 || max_value < n then invalid_arg "invalid initial count"
  else Awaitable.make ?padded (n * one lor no_awaiters)

let release t =
  let prior = Awaitable.fetch_and_add t one in
  if prior < -poisoned / 2 || max_value * one <= prior then
    (* In case of poisoning, the count should be sufficiently negative that it
       is impossible to make it greater than [-poisoned / 2] even when all
       threads [release] concurrently.

       In case of overflow, the count should be sufficiently large that it is
       impossible for an acquirer to take the extra resource before we decrement
       it back. *)
    let undo_release t =
      let prior = Awaitable.fetch_and_add t (-one) in
      if 0 < prior then raise (Sys_error "overflow")
    in
    undo_release t
  else if prior land no_awaiters = 0 then
    let signal_awaiter t before =
      Awaitable.compare_and_set t before (before lor no_awaiters) |> ignore;
      Awaitable.signal t
    in
    signal_awaiter t (prior + one)

let[@inline] acquire t =
  let prior = Awaitable.fetch_and_add t (-one) in
  if prior < one then
    let[@inline never] rec acquire_contended t before =
      if before <= -one lor no_awaiters then
        let after = (before + one) land lnot no_awaiters in
        if Awaitable.compare_and_set t before after then
          let rec acquire_awaiting t before =
            if one <= before then begin
              let after = (before - one) land lnot no_awaiters in
              if Awaitable.compare_and_set t before after then begin
                if one <= after then Awaitable.signal t
              end
              else acquire_awaiting t (Awaitable.get t)
            end
            else if before < -poisoned / 2 then raise Poisoned
            else
              let after = before land lnot no_awaiters in
              if before = after || Awaitable.compare_and_set t before after then
                Awaitable.await t after;
              acquire_awaiting t (Awaitable.get t)
          in
          acquire_awaiting t after
        else acquire_contended t (Awaitable.get t)
    in
    acquire_contended t (prior - one)

let try_acquire t =
  let prior = Awaitable.fetch_and_add t (-one) in
  one <= prior
  ||
  let undo_acquire t =
    let prior = Awaitable.fetch_and_add t one in
    if prior < -poisoned / 2 then raise Poisoned
    else begin
      if 0 <= prior then Awaitable.signal t;
      false
    end
  in
  undo_acquire t

let rec poison t =
  let before = Awaitable.get t in
  if -poisoned / 2 < before then
    let after = -poisoned lor no_awaiters in
    if Awaitable.compare_and_set t before after then Awaitable.broadcast t
    else poison t

let get_value t =
  let n = Awaitable.get t asr one_shift in
  if 0 <= n then if n < max_value then n else max_value else 0

let is_poisoned t = Awaitable.get t < -poisoned / 2
