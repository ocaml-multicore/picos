open Picos

let[@inline never] owner () = raise (Sys_error "Mutex: owner")
let[@inline never] unlocked () = raise (Sys_error "Mutex: unlocked")
let[@inline never] not_owner () = raise (Sys_error "Mutex: not owner")

type entry = { trigger : Trigger.t; fiber : Fiber.Maybe.t }

type state =
  | Unlocked
  | Locked of { fiber : Fiber.Maybe.t; head : entry list; tail : entry list }

type t = state Atomic.t

let create ?(padded = false) () =
  let t = Atomic.make Unlocked in
  if padded then Multicore_magic.copy_as_padded t else t

(* We try to avoid starvation of unlock by making it so that when, at the start
   of lock or unlock, the head is empty, the tail is reversed into the head.
   This way both lock and unlock attempt O(1) and O(n) operations at the same
   time. *)

let locked_nothing =
  Locked { fiber = Fiber.Maybe.nothing; head = []; tail = [] }

let rec unlock_as owner t backoff =
  match Atomic.get t with
  | Unlocked -> unlocked ()
  | Locked r as before ->
      if Fiber.Maybe.equal r.fiber owner then
        match r.head with
        | { trigger; fiber } :: rest ->
            let after = Locked { r with fiber; head = rest } in
            transfer_as owner t backoff before after trigger
        | [] -> begin
            match List.rev r.tail with
            | { trigger; fiber } :: rest ->
                let after = Locked { fiber; head = rest; tail = [] } in
                transfer_as owner t backoff before after trigger
            | [] ->
                if not (Atomic.compare_and_set t before Unlocked) then
                  unlock_as owner t (Backoff.once backoff)
          end
      else not_owner ()

and transfer_as owner t backoff before after trigger =
  if Atomic.compare_and_set t before after then Trigger.signal trigger
  else unlock_as owner t (Backoff.once backoff)

let[@inline] unlock ?checked t =
  let owner = Fiber.Maybe.current_if checked in
  unlock_as owner t Backoff.default

let rec cleanup_as entry t backoff =
  (* We have been canceled.  If we are the owner, we must unlock the mutex.
     Otherwise we must remove our entry from the queue. *)
  match Atomic.get t with
  | Locked r as before ->
      (* At this point we must use strict physical equality [==] rather than
         [Fiber.Maybe.equal]! *)
      if r.fiber == entry.fiber then unlock_as entry.fiber t backoff
      else if r.head != [] then
        match List_ext.drop_first_or_not_found entry r.head with
        | head ->
            let after = Locked { r with head } in
            cancel_as entry t backoff before after
        | exception Not_found ->
            let tail = List_ext.drop_first_or_not_found entry r.tail in
            let after = Locked { r with tail } in
            cancel_as entry t backoff before after
      else
        let tail =
          List_ext.drop_first_or_not_found entry r.tail (* wont raise *)
        in
        let after = Locked { r with tail } in
        cancel_as entry t backoff before after
  | Unlocked -> unlocked () (* impossible *)

and cancel_as fiber t backoff before after =
  if not (Atomic.compare_and_set t before after) then
    cleanup_as fiber t (Backoff.once backoff)

let rec lock_as fiber t backoff =
  match Atomic.get t with
  | Unlocked as before ->
      let after =
        if fiber == Fiber.Maybe.nothing then locked_nothing
        else Locked { fiber; head = []; tail = [] }
      in
      if not (Atomic.compare_and_set t before after) then
        lock_as fiber t (Backoff.once backoff)
  | Locked r as before ->
      let fiber = Fiber.Maybe.or_current fiber in
      if Fiber.Maybe.unequal r.fiber fiber then
        let trigger = Trigger.create () in
        let entry = { trigger; fiber } in
        let after =
          if r.head == [] then
            Locked { r with head = List.rev_append r.tail [ entry ]; tail = [] }
          else Locked { r with tail = entry :: r.tail }
        in
        if Atomic.compare_and_set t before after then begin
          match Trigger.await trigger with
          | None -> ()
          | Some exn_bt ->
              cleanup_as entry t Backoff.default;
              Exn_bt.raise exn_bt
        end
        else lock_as fiber t (Backoff.once backoff)
      else owner ()

let[@inline] lock ?checked t =
  let fiber = Fiber.Maybe.current_and_check_if checked in
  lock_as fiber t Backoff.default

let try_lock ?checked t =
  let fiber = Fiber.Maybe.current_and_check_if checked in
  Atomic.get t == Unlocked
  && Atomic.compare_and_set t Unlocked
       (if fiber == Fiber.Maybe.nothing then locked_nothing
        else Locked { fiber; head = []; tail = [] })

let protect ?checked t body =
  let fiber = Fiber.Maybe.current_and_check_if checked in
  lock_as fiber t Backoff.default;
  match body () with
  | value ->
      unlock_as fiber t Backoff.default;
      value
  | exception exn ->
      let bt = Printexc.get_raw_backtrace () in
      unlock_as fiber t Backoff.default;
      Printexc.raise_with_backtrace exn bt
