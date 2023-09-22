open Picos

module Make (Mutex : Sync_intf.Mutex) :
  Sync_intf.Condition with type mutex := Mutex.t = struct
  type state = { head : Trigger.as_signal list; tail : Trigger.as_signal list }

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
      if before == empty then
        { head = [ (trigger :> Trigger.as_signal) ]; tail = [] }
      else if before.head != [] then
        { before with tail = (trigger :> Trigger.as_signal) :: before.tail }
      else
        let head =
          List.rev_append [ (trigger :> Trigger.as_signal) ] before.tail
        in
        { head; tail = [] }
    in
    if Atomic.compare_and_set t before after then begin
      match Mutex.succumb mutex @@ fun () -> Trigger.await trigger with
      | None -> ()
      | Some exn_bt ->
          cleanup Backoff.default (trigger :> Trigger.as_signal) t;
          Exn_bt.raise exn_bt
    end
    else wait (Backoff.once backoff) trigger t mutex

  let wait t mutex = wait Backoff.default (Trigger.create ()) t mutex
end
