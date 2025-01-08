open Picos_std_awaitable

let poisoned = 1 lsl 0
let writers = 1 lsl 1
let locked = 1 lsl 2

type t = int Awaitable.t

exception Poisoned

let poison t =
  let before = Awaitable.get t in
  if before < locked then invalid_arg "not locked";
  (* Unfortunately we cannot check ownership at this point. *)
  if before land poisoned = 0 then
    let _ : int = Awaitable.fetch_and_add t poisoned in
    Awaitable.broadcast t

let rec lock_awaiting t =
  let before = Awaitable.get t in
  if before < locked then begin
    let after = locked lor writers in
    if not (Awaitable.compare_and_set t before after) then lock_awaiting t
  end
  else if before land poisoned <> 0 then raise Poisoned
  else
    let after = before lor writers in
    if before = after || Awaitable.compare_and_set t before after then
      Awaitable.await t after;
    lock_awaiting t

let rec lock_contended t =
  let before = Awaitable.get t in
  if locked * 2 <= before then
    let after = (before - locked) lor writers in
    if Awaitable.compare_and_set t before after then lock_awaiting t
    else lock_contended t

let rec signal_awaiter t =
  let before = Awaitable.get t in
  if before < locked && writers <= before then
    let after = (* can't be poisoned *) 0 in
    if Awaitable.compare_and_set t before after then Awaitable.signal t
    else signal_awaiter t

let lock t =
  let prior = Awaitable.fetch_and_add t locked in
  if locked <= prior then lock_contended t

let unlock t =
  let prior = Awaitable.fetch_and_add t (-locked) in
  if locked lor writers <= prior then signal_awaiter t

let protect t thunk =
  lock t;
  match thunk () with
  | value ->
      unlock t;
      value
  | exception exn ->
      let bt = Printexc.get_raw_backtrace () in
      poison t;
      Printexc.raise_with_backtrace exn bt

let[@inline] create ?padded () = Awaitable.make ?padded 0
let[@inline] is_locked t = locked <= Awaitable.get t
let[@inline] is_poisoned t = Awaitable.get t land poisoned <> 0

module Condition = struct
  include Cond

  let wait condition mutex = wait condition mutex ~lock ~unlock
end
