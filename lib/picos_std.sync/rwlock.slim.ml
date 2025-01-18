open Picos_std_awaitable

(** TODO: Should we try to prevent writers from starvation? *)

type t = int Awaitable.t

let frozen = 1 lsl 0
let poisoned = 1 lsl 1
let no_writers = 1 lsl 2
let no_readers = 1 lsl 3
let no_awaiters = no_readers lor no_writers
let locked = 1 lsl 4
let exclusive = 1 lsl (Sys.int_size - 2)

(* *)

exception Poisoned
exception Frozen

let rec lock_awaiting t =
  let before = Awaitable.get t in
  if before < locked then begin
    let after = before land lnot no_writers lor locked lor exclusive in
    if not (Awaitable.compare_and_set t before after) then lock_awaiting t
  end
  else if before land (poisoned lor frozen) <> 0 then
    raise (if before land poisoned <> 0 then Poisoned else Frozen)
  else
    let after = before land lnot no_writers in
    if before = after || Awaitable.compare_and_set t before after then
      Awaitable.await t after;
    lock_awaiting t

let lock t =
  let before = Awaitable.get t in
  if
    locked <= before
    ||
    let after = before lor locked lor exclusive in
    not (Awaitable.compare_and_set t before after)
  then lock_awaiting t

let rec lock_ro_awaiting t =
  let before = Awaitable.get t in
  if before < exclusive then begin
    let after = (before + locked) land lnot no_readers in
    if not (Awaitable.compare_and_set t before after) then lock_ro_awaiting t
  end
  else if before land poisoned <> 0 then raise Poisoned
  else
    let after = before land lnot no_readers in
    if before = after || Awaitable.compare_and_set t before after then
      Awaitable.await t after;
    lock_ro_awaiting t

let rec lock_ro_contended t =
  let before = Awaitable.get t in
  if locked * 2 <= before then
    let after = (before - locked) land lnot no_readers in
    if Awaitable.compare_and_set t before after then lock_ro_awaiting t
    else lock_ro_contended t

let lock_ro t =
  let prior = Awaitable.fetch_and_add t locked in
  if exclusive <= prior then lock_ro_contended t

let rec signal_awaiters t =
  let before = Awaitable.get t in
  if before < no_awaiters then
    if Awaitable.compare_and_set t before (before lor no_awaiters) then
      if no_readers <= before then Awaitable.signal t else Awaitable.broadcast t
    else signal_awaiters t

let unlock t =
  let before = Awaitable.get t in
  if exclusive <= before then begin
    let prior =
      Awaitable.fetch_and_add t
        ((-(locked lor exclusive) lor no_awaiters) - (before land no_awaiters))
    in
    if prior < locked lor exclusive lor no_awaiters then
      if locked lor exclusive lor no_readers <= prior then Awaitable.signal t
      else Awaitable.broadcast t
  end
  else
    let prior = Awaitable.fetch_and_add t (-locked) in
    if prior < locked lor no_awaiters then signal_awaiters t

let poison t =
  let before = Awaitable.get t in
  if before land exclusive = 0 then invalid_arg "not write locked";
  (* Unfortunately we cannot check ownership at this point. *)
  if before land poisoned = 0 then
    let prior = Awaitable.fetch_and_add t poisoned in
    if prior land no_awaiters <> no_awaiters then Awaitable.broadcast t

let freeze t =
  lock_ro t;
  let before = ref (Awaitable.get t) in
  while
    !before land frozen = 0
    &&
    let after = !before + (locked lor frozen) in
    (* This leaves the rwlock as read locked. *)
    not (Awaitable.compare_and_set t !before after)
  do
    before := Awaitable.get t
  done;
  (* We must wake up any writers waiting to obtain the lock. *)
  if !before land no_awaiters <> no_awaiters then Awaitable.broadcast t;
  unlock t

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

let protect_ro t thunk =
  lock_ro t;
  match thunk () with
  | value ->
      unlock t;
      value
  | exception exn ->
      let bt = Printexc.get_raw_backtrace () in
      unlock t;
      Printexc.raise_with_backtrace exn bt

let[@inline] create ?padded () =
  Awaitable.make ?padded (no_readers lor no_writers)

let[@inline] is_locked t = exclusive <= Awaitable.get t
let[@inline] is_poisoned t = Awaitable.get t land poisoned <> 0
let[@inline] is_frozen t = Awaitable.get t land frozen <> 0

module Condition = struct
  include Cond

  let wait condition mutex = wait condition mutex ~lock ~unlock
end
