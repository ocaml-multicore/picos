open Picos

type state = Empty | Queue of { head : Trigger.t list; tail : Trigger.t list }
type t = state Atomic.t

let create ?padded () = Multicore_magic.copy_as ?padded @@ Atomic.make Empty

let broadcast t =
  if Atomic.get t != Empty then
    match Atomic.exchange t Empty with
    | Empty -> ()
    | Queue r ->
        List.iter Trigger.signal r.head;
        List.iter Trigger.signal (List.rev r.tail)

(* We try to avoid starvation of signal by making it so that when, at the start
   of signal or wait, the head is empty, the tail is reversed into the head.
   This way both signal and wait attempt O(1) and O(n) operations at the same
   time. *)

let rec signal t backoff =
  match Atomic.get t with
  | Empty -> ()
  | Queue r as before -> begin
      match r.head with
      | trigger :: head ->
          signal_cas t backoff before
            (if head == [] && r.tail == [] then Empty else Queue { r with head })
            trigger
      | [] -> begin
          match List.rev r.tail with
          | trigger :: head ->
              signal_cas t backoff before
                (if head == [] then Empty else Queue { head; tail = [] })
                trigger
          | [] -> failwith "impossible"
        end
    end

and signal_cas t backoff before after trigger =
  if Atomic.compare_and_set t before after then Trigger.signal trigger
  else signal t (Backoff.once backoff)

let signal t = signal t Backoff.default

let rec cleanup backoff trigger t =
  (* We have been canceled.  If we can't drop our trigger from the variable, we
     signal the next trigger in queue to make sure each signal wakes up at least
     one non-canceled waiter if possible. *)
  match Atomic.get t with
  | Empty -> ()
  | Queue r as before -> begin
      if r.head != [] then
        match List_ext.drop_first_or_not_found trigger r.head with
        | head ->
            cleanup_cas backoff trigger t before
              (if head == [] && r.tail == [] then Empty
               else Queue { r with head })
        | exception Not_found -> begin
            match List_ext.drop_first_or_not_found trigger r.tail with
            | tail ->
                cleanup_cas backoff trigger t before (Queue { r with tail })
            | exception Not_found -> signal t
          end
      else
        match List_ext.drop_first_or_not_found trigger r.tail with
        | tail ->
            cleanup_cas backoff trigger t before
              (if tail == [] then Empty else Queue { head = []; tail })
        | exception Not_found -> signal t
    end

and cleanup_cas backoff trigger t before after =
  if not (Atomic.compare_and_set t before after) then
    cleanup (Backoff.once backoff) trigger t

let rec wait t mutex trigger fiber backoff =
  let before = Atomic.get t in
  let after =
    match before with
    | Empty -> Queue { head = [ trigger ]; tail = [] }
    | Queue r ->
        if r.head != [] then Queue { r with tail = trigger :: r.tail }
        else Queue { head = List.rev_append r.tail [ trigger ]; tail = [] }
  in
  if Atomic.compare_and_set t before after then begin
    Mutex.unlock_as (Fiber.Maybe.of_fiber fiber) mutex Backoff.default;
    let result = Trigger.await trigger in
    let forbid = Fiber.exchange fiber ~forbid:true in
    Mutex.lock_as (Fiber.Maybe.of_fiber fiber) mutex Nothing Backoff.default;
    Fiber.set fiber ~forbid;
    match result with
    | None -> ()
    | Some exn_bt ->
        cleanup Backoff.default trigger t;
        Exn_bt.raise exn_bt
  end
  else wait t mutex trigger fiber (Backoff.once backoff)

let wait t mutex =
  let fiber = Fiber.current () in
  let trigger = Trigger.create () in
  wait t mutex trigger fiber Backoff.default
