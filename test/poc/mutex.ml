open Picos

module Checked = struct
  type entry = { trigger : Trigger.as_signal; fiber : Fiber.as_async }

  type state =
    | Unlocked
    | Locked of { fiber : Fiber.as_async; head : entry list; tail : entry list }

  type t = state Atomic.t

  let create () = Atomic.make Unlocked
  let[@inline never] owner () = raise (Sys_error "Mutex: owner")
  let[@inline never] unlocked () = raise (Sys_error "Mutex: unlocked")
  let[@inline never] not_owner () = raise (Sys_error "Mutex: not owner")

  (* We try to avoid starvation of unlock by making it so that when, at the start
     of lock or unlock, the head is empty, the tail is reversed into the head.
     This way both lock and unlock attempt O(1) and O(n) operations at the same
     time. *)

  let rec unlock_as backoff owner t =
    match Atomic.get t with
    | Unlocked -> unlocked ()
    | Locked r as before ->
        if Fiber.equal r.fiber owner then
          match r.head with
          | { trigger; fiber } :: rest ->
              let after = Locked { r with fiber; head = rest } in
              transfer_as backoff owner t before after trigger
          | [] -> begin
              match List.rev r.tail with
              | { trigger; fiber } :: rest ->
                  let after = Locked { fiber; head = rest; tail = [] } in
                  transfer_as backoff owner t before after trigger
              | [] ->
                  if not (Atomic.compare_and_set t before Unlocked) then
                    unlock_as (Backoff.once backoff) owner t
            end
        else not_owner ()

  and transfer_as backoff owner t before after trigger =
    if Atomic.compare_and_set t before after then Trigger.signal trigger
    else unlock_as (Backoff.once backoff) owner t

  let unlock t =
    unlock_as Backoff.default (Fiber.current () :> Fiber.as_async) t

  let rec cleanup_as backoff entry t =
    (* We have been canceled.  If we are the owner, we must unlock the mutex.
       Otherwise we must remove our entry from the queue. *)
    match Atomic.get t with
    | Locked r as before ->
        if Fiber.equal r.fiber entry.fiber then unlock_as backoff entry.fiber t
        else if r.head != [] then
          match List.drop_first_or_not_found entry r.head with
          | head ->
              let after = Locked { r with head } in
              cancel_as backoff entry t before after
          | exception Not_found ->
              let tail = List.drop_first_or_not_found entry r.tail in
              let after = Locked { r with tail } in
              cancel_as backoff entry t before after
        else
          let tail =
            List.drop_first_or_not_found entry r.tail (* wont raise *)
          in
          let after = Locked { r with tail } in
          cancel_as backoff entry t before after
    | Unlocked -> unlocked () (* impossible *)

  and cancel_as backoff fiber t before after =
    if not (Atomic.compare_and_set t before after) then
      cleanup_as (Backoff.once backoff) fiber t

  let rec lock_as backoff fiber t =
    match Atomic.get t with
    | Unlocked as before ->
        let after = Locked { fiber; head = []; tail = [] } in
        if not (Atomic.compare_and_set t before after) then
          lock_as (Backoff.once backoff) fiber t
    | Locked r as before ->
        if Fiber.equal r.fiber (fiber :> Fiber.as_async) then owner ()
        else
          let trigger = Trigger.create () in
          let entry = { trigger :> Trigger.as_signal; fiber } in
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
                cleanup_as Backoff.default entry t;
                Exn_bt.raise exn_bt
          end
          else lock_as (Backoff.once backoff) fiber t

  let lock t = lock_as Backoff.default (Fiber.current () :> Fiber.as_async) t

  let try_lock t =
    let fiber = (Fiber.current () :> Fiber.as_async) in
    Atomic.get t == Unlocked
    && Atomic.compare_and_set t Unlocked
         (Locked { fiber; head = []; tail = [] })

  let protect t body =
    let fiber = (Fiber.current () :> Fiber.as_async) in
    lock_as Backoff.default fiber t;
    match body () with
    | value ->
        unlock_as Backoff.default fiber t;
        value
    | exception exn ->
        let bt = Printexc.get_raw_backtrace () in
        unlock_as Backoff.default fiber t;
        Printexc.raise_with_backtrace exn bt

  let succumb t body =
    let fiber = Fiber.current () in
    unlock_as Backoff.default (fiber :> Fiber.as_async) t;
    match body () with
    | value ->
        Fiber.forbid fiber (fun () ->
            lock_as Backoff.default (fiber :> Fiber.as_async) t);
        value
    | exception exn ->
        let bt = Printexc.get_raw_backtrace () in
        Fiber.forbid fiber (fun () ->
            lock_as Backoff.default (fiber :> Fiber.as_async) t);
        Printexc.raise_with_backtrace exn bt
end
