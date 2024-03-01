open Picos
open Foundation

module Checked = struct
  type entry = { trigger : Trigger.t; fiber : Fiber.t }

  type state =
    | Unlocked
    | Locked of { fiber : Fiber.t; head : entry list; tail : entry list }

  type t = state Atomic.t

  let create () = Atomic.make Unlocked
  let[@inline never] owner () = raise (Sys_error "Mutex: owner")
  let[@inline never] unlocked () = raise (Sys_error "Mutex: unlocked")
  let[@inline never] not_owner () = raise (Sys_error "Mutex: not owner")

  (* We try to avoid starvation of unlock by making it so that when, at the start
     of lock or unlock, the head is empty, the tail is reversed into the head.
     This way both lock and unlock attempt O(1) and O(n) operations at the same
     time. *)

  let rec unlock_as t owner backoff =
    match Atomic.get t with
    | Unlocked -> unlocked ()
    | Locked r as before ->
        if Fiber.equal r.fiber owner then
          match r.head with
          | { trigger; fiber } :: rest ->
              let after = Locked { r with fiber; head = rest } in
              transfer_as t owner before after trigger backoff
          | [] -> begin
              match List.rev r.tail with
              | { trigger; fiber } :: rest ->
                  let after = Locked { fiber; head = rest; tail = [] } in
                  transfer_as t owner before after trigger backoff
              | [] ->
                  if not (Atomic.compare_and_set t before Unlocked) then
                    unlock_as t owner (Backoff.once backoff)
            end
        else not_owner ()

  and transfer_as t owner before after trigger backoff =
    if Atomic.compare_and_set t before after then Trigger.signal trigger
    else unlock_as t owner (Backoff.once backoff)

  let unlock t = unlock_as t (Fiber.current ()) Backoff.default

  let rec cleanup_as t entry backoff =
    (* We have been canceled.  If we are the owner, we must unlock the mutex.
       Otherwise we must remove our entry from the queue. *)
    match Atomic.get t with
    | Locked r as before ->
        if Fiber.equal r.fiber entry.fiber then unlock_as t entry.fiber backoff
        else if r.head != [] then
          match List.drop_first_or_not_found entry r.head with
          | head ->
              let after = Locked { r with head } in
              cancel_as t entry before after backoff
          | exception Not_found ->
              let tail = List.drop_first_or_not_found entry r.tail in
              let after = Locked { r with tail } in
              cancel_as t entry before after backoff
        else
          let tail =
            List.drop_first_or_not_found entry r.tail (* wont raise *)
          in
          let after = Locked { r with tail } in
          cancel_as t entry before after backoff
    | Unlocked -> unlocked () (* impossible *)

  and cancel_as t fiber before after backoff =
    if not (Atomic.compare_and_set t before after) then
      cleanup_as t fiber (Backoff.once backoff)

  let rec lock_as t fiber backoff =
    match Atomic.get t with
    | Unlocked as before ->
        let after = Locked { fiber; head = []; tail = [] } in
        if not (Atomic.compare_and_set t before after) then
          lock_as t fiber (Backoff.once backoff)
    | Locked r as before ->
        if Fiber.equal r.fiber fiber then owner ()
        else
          let trigger = Trigger.create () in
          let entry = { trigger; fiber } in
          let after =
            if r.head == [] then
              Locked
                { r with head = List.rev_append [ entry ] r.tail; tail = [] }
            else Locked { r with tail = entry :: r.tail }
          in
          if Atomic.compare_and_set t before after then begin
            match Trigger.await trigger with
            | None -> ()
            | Some exn_bt ->
                cleanup_as t entry Backoff.default;
                Exn_bt.raise exn_bt
          end
          else lock_as t fiber (Backoff.once backoff)

  let lock t = lock_as t (Fiber.current ()) Backoff.default

  let try_lock t =
    let fiber = Fiber.current () in
    Atomic.get t == Unlocked
    && Atomic.compare_and_set t Unlocked
         (Locked { fiber; head = []; tail = [] })

  let protect t body =
    let fiber = Fiber.current () in
    lock_as t fiber Backoff.default;
    match body () with
    | value ->
        unlock_as t fiber Backoff.default;
        value
    | exception exn ->
        let bt = Printexc.get_raw_backtrace () in
        unlock_as t fiber Backoff.default;
        Printexc.raise_with_backtrace exn bt

  let succumb t body =
    let fiber = Fiber.current () in
    unlock_as t fiber Backoff.default;
    match body () with
    | value ->
        Fiber.forbid fiber (fun () -> lock_as t fiber Backoff.default);
        value
    | exception exn ->
        let bt = Printexc.get_raw_backtrace () in
        Fiber.forbid fiber (fun () -> lock_as t fiber Backoff.default);
        Printexc.raise_with_backtrace exn bt
end

module Make (Mutex : Sync_intf.Mutex) :
  Sync_intf.Condition with type mutex := Mutex.t = struct
  type state = { head : Trigger.t list; tail : Trigger.t list }

  let empty = { head = []; tail = [] }

  type t = state Atomic.t

  let create () = Atomic.make empty

  let broadcast t =
    if Atomic.get t != empty then begin
      let state = Atomic.exchange t empty in
      List.iter Trigger.signal state.head;
      List.iter Trigger.signal (List.rev state.tail)
    end

  let update_head before head =
    if head == [] && before.tail == [] then empty else { before with head }

  let[@inline] of_head head = if head == [] then empty else { head; tail = [] }
  let[@inline] of_tail tail = if tail == [] then empty else { head = []; tail }

  (* We try to avoid starvation of signal by making it so that when, at the start
     of signal or wait, the head is empty, the tail is reversed into the head.
     This way both signal and wait attempt O(1) and O(n) operations at the same
     time. *)

  let rec signal backoff t =
    let before = Atomic.get t in
    if before != empty then
      match before.head with
      | trigger :: head ->
          signal_cas backoff t before (update_head before head) trigger
      | [] -> begin
          match List.rev before.tail with
          | trigger :: head ->
              signal_cas backoff t before (of_head head) trigger
          | [] -> failwith "impossible"
        end

  and signal_cas backoff t before after trigger =
    if Atomic.compare_and_set t before after then Trigger.signal trigger
    else signal (Backoff.once backoff) t

  let signal t = signal Backoff.default t

  let rec cleanup backoff trigger t =
    (* We have been canceled.  If we can't drop our trigger from the variable, we
       signal the next trigger in queue to make sure each signal wakes up at least
       one non-canceled waiter if possible. *)
    let before = Atomic.get t in
    if before != empty then
      if before.head != [] then
        match List.drop_first_or_not_found trigger before.head with
        | head -> cleanup_cas backoff trigger t before (update_head before head)
        | exception Not_found -> begin
            match List.drop_first_or_not_found trigger before.tail with
            | tail -> cleanup_cas backoff trigger t before { before with tail }
            | exception Not_found -> signal t
          end
      else
        match List.drop_first_or_not_found trigger before.tail with
        | tail -> cleanup_cas backoff trigger t before (of_tail tail)
        | exception Not_found -> signal t

  and cleanup_cas backoff trigger t before after =
    if not (Atomic.compare_and_set t before after) then
      cleanup (Backoff.once backoff) trigger t

  let rec wait backoff trigger t mutex =
    let before = Atomic.get t in
    let after =
      if before == empty then { head = [ trigger ]; tail = [] }
      else if before.head != [] then
        { before with tail = trigger :: before.tail }
      else
        let head = List.rev_append [ trigger ] before.tail in
        { head; tail = [] }
    in
    if Atomic.compare_and_set t before after then begin
      match Mutex.succumb mutex @@ fun () -> Trigger.await trigger with
      | None -> ()
      | Some exn_bt ->
          cleanup Backoff.default trigger t;
          Exn_bt.raise exn_bt
    end
    else wait (Backoff.once backoff) trigger t mutex

  let wait t mutex = wait Backoff.default (Trigger.create ()) t mutex
end
