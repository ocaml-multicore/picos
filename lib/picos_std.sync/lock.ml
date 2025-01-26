open Picos_std_awaitable

let no_awaiters = 1 lsl 0
let locked = 1 lsl 1
let locked_permanently = 1 lsl (Sys.int_size - 2)

type t = int Awaitable.t

exception Poisoned

let rec poison t =
  let before = Awaitable.get t in
  if before < locked then invalid_arg "not locked";
  (* Unfortunately we cannot check ownership at this point. *)
  if before < locked_permanently / 2 then
    if Awaitable.compare_and_set t before (before + locked_permanently) then
      Awaitable.broadcast t
    else poison t

let acquire t =
  let prior = Awaitable.fetch_and_add t locked in
  if locked <= prior then
    let rec acquire_awaiting t before =
      if before < locked then begin
        (* We know [before] is [0] or [no_awaiters]. *)
        let after = locked land lnot no_awaiters in
        if not (Awaitable.compare_and_set t before after) then
          acquire_awaiting t (Awaitable.get t)
      end
      else if locked_permanently / 2 <= before then raise Poisoned
      else
        let after = before land lnot no_awaiters in
        if before = after || Awaitable.compare_and_set t before after then
          Awaitable.await t after;
        acquire_awaiting t (Awaitable.get t)
    in
    let rec acquire_contended t before =
      if locked * 2 <= before then
        let after = (before - locked) land lnot no_awaiters in
        if Awaitable.compare_and_set t before after then
          acquire_awaiting t after
        else acquire_contended t (Awaitable.get t)
    in
    acquire_contended t (prior + locked)

let release t =
  let prior = Awaitable.fetch_and_add t (-locked) in
  if prior < locked lor no_awaiters then
    let signal_awaiter t =
      (* Lock state was [0] after the [fetch_and_add] in [unlock]. *)
      if Awaitable.compare_and_set t 0 no_awaiters then Awaitable.signal t
    in
    signal_awaiter t
  else if locked_permanently / 2 <= prior then
    let undo_release t =
      let _ : int = Awaitable.fetch_and_add t locked in
      ()
    in
    undo_release t

let holding t thunk = Locks.holding t thunk ~acquire ~release ~poison
let protect t thunk = Locks.protect t thunk ~acquire ~release

let try_acquire t =
  let prior = Awaitable.fetch_and_add t locked in
  prior < locked
  ||
  let undo_acquire t =
    let prior = Awaitable.fetch_and_add t (-locked) in
    if locked_permanently / 2 <= prior then raise Poisoned
    else begin
      if prior < locked lor no_awaiters then Awaitable.signal t;
      false
    end
  in
  undo_acquire t

let[@inline] create ?padded () = Awaitable.make ?padded no_awaiters
let[@inline] is_locked t = locked <= Awaitable.get t
let[@inline] is_poisoned t = locked_permanently / 2 <= Awaitable.get t

module Condition = struct
  type lock = t

  include Conditions

  let wait condition mutex = wait condition mutex ~acquire ~release
end
