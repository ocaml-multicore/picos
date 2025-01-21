open Picos_std_awaitable

(** TODO: Should we try to prevent writers from starvation? *)

type t = int Awaitable.t

let no_writers = 1 lsl 0
let no_readers = 1 lsl 1
let no_awaiters = no_readers lor no_writers
let shared = 1 lsl 2
let shared_permanently = 1 lsl (Sys.int_size - 5)
let exclusive = 1 lsl (Sys.int_size - 4)
let exclusive_permanently = 1 lsl (Sys.int_size - 2)

exception Poisoned
exception Frozen

let acquire t =
  let before = no_awaiters in
  let after = exclusive lor no_awaiters in
  if not (Awaitable.compare_and_set t before after) then
    let rec acquire_awaiting t =
      let before = Awaitable.get t in
      if before < shared then begin
        let after = before land lnot no_writers lor exclusive in
        if not (Awaitable.compare_and_set t before after) then
          acquire_awaiting t
      end
      else if shared_permanently / 2 <= before land lnot exclusive then
        raise (if exclusive < before then Poisoned else Frozen)
      else
        let after = before land lnot no_writers in
        if before = after || Awaitable.compare_and_set t before after then
          Awaitable.await t after;
        acquire_awaiting t
    in
    acquire_awaiting t

let release t =
  let before = exclusive lor no_awaiters in
  let after = no_awaiters in
  if not (Awaitable.compare_and_set t before after) then
    let rec release_contended t =
      let before = Awaitable.get t in
      if before land lnot exclusive < shared_permanently / 2 then
        let after = before land lnot exclusive lor no_awaiters in
        if Awaitable.compare_and_set t before after then begin
          if before land no_awaiters <> no_awaiters then
            if before land no_readers = 0 then Awaitable.broadcast t
            else Awaitable.signal t
        end
        else release_contended t
    in
    release_contended t

let acquire_shared t =
  let prior = Awaitable.fetch_and_add t shared in
  if exclusive <= prior then
    let rec acquire_shared_awaiting t =
      let before = Awaitable.get t in
      if before < exclusive then begin
        let after = (before + shared) land lnot no_readers in
        if not (Awaitable.compare_and_set t before after) then
          acquire_shared_awaiting t
      end
      else if exclusive_permanently / 2 <= before then raise Poisoned
      else
        let after = before land lnot no_readers in
        if before = after || Awaitable.compare_and_set t before after then
          Awaitable.await t after;
        acquire_shared_awaiting t
    in
    let rec acquire_shared_contended t =
      let before = Awaitable.get t in
      if exclusive <= before then
        let after = (before - shared) land lnot no_readers in
        if Awaitable.compare_and_set t before after then
          acquire_shared_awaiting t
        else acquire_shared_contended t
    in
    acquire_shared_contended t

let release_shared t =
  let prior = Awaitable.fetch_and_add t (-shared) in
  if prior < shared lor no_awaiters then
    let rec signal_awaiters t =
      let before = Awaitable.get t in
      if before < no_awaiters then
        if Awaitable.compare_and_set t before (before lor no_awaiters) then
          if no_readers <= before then Awaitable.signal t
          else Awaitable.broadcast t
        else signal_awaiters t
    in
    signal_awaiters t
  else if exclusive_permanently / 2 < prior then
    let undo_release t =
      let _ : int = Awaitable.fetch_and_add t shared in
      ()
    in
    undo_release t

let rec poison t =
  let before = Awaitable.get t in
  if before < exclusive then invalid_arg "not write locked";
  (* Unfortunately we cannot check ownership at this point. *)
  if before < exclusive_permanently / 2 then
    let after = exclusive_permanently lor no_awaiters in
    if Awaitable.compare_and_set t before after then Awaitable.broadcast t
    else poison t

let freeze t =
  acquire_shared t;
  let before = ref (Awaitable.get t) in
  while
    !before < shared_permanently / 2
    &&
    let after = (!before + shared_permanently) lor no_awaiters in
    (* This leaves the rwlock as read locked. *)
    not (Awaitable.compare_and_set t !before after)
  do
    before := Awaitable.get t
  done;
  (* We must wake up any writers waiting to obtain the lock. *)
  if !before land no_awaiters <> no_awaiters then Awaitable.broadcast t;
  release_shared t

let holding t thunk = Locks.holding t thunk ~acquire ~release ~poison
let protect t thunk = Locks.protect t thunk ~acquire ~release

let sharing t thunk =
  acquire_shared t;
  match thunk () with
  | value ->
      release_shared t;
      value
  | exception exn ->
      let bt = Printexc.get_raw_backtrace () in
      release_shared t;
      Printexc.raise_with_backtrace exn bt

let try_acquire t =
  let before = Awaitable.get t in
  if before < shared then
    Awaitable.compare_and_set t before (before lor exclusive)
  else
    shared_permanently / 2 <= before land lnot exclusive
    && raise (if exclusive < before then Poisoned else Frozen)

let try_acquire_shared t =
  let before = Awaitable.get t in
  if before < exclusive then Awaitable.compare_and_set t before (before + shared)
  else exclusive_permanently / 2 <= before && raise Poisoned

let[@inline] create ?padded () = Awaitable.make ?padded no_awaiters
let[@inline] is_locked t = exclusive <= Awaitable.get t

let[@inline] is_frozen t =
  let state = Awaitable.get t in
  shared_permanently / 2 <= state && state < exclusive

let[@inline] is_poisoned t = exclusive_permanently / 2 <= Awaitable.get t

let[@inline] is_locked_shared t =
  let state = Awaitable.get t in
  shared <= state && state < exclusive

module Condition = struct
  type lock = t

  include Conditions

  let wait_shared condition lock =
    wait condition lock ~acquire:acquire_shared ~release:release_shared

  let wait condition lock = wait condition lock ~acquire ~release
end
