open Picos

type t = Trigger.t Q.t Atomic.t

let create ?padded () =
  Multicore_magic.copy_as ?padded @@ Atomic.make (Q.T Zero)

let broadcast (t : t) =
  if Atomic.get t != T Zero then
    match Atomic.exchange t (T Zero) with
    | T Zero -> ()
    | T (One _ as q) -> Q.iter Trigger.signal q

(* We try to avoid starvation of signal by making it so that when, at the start
   of signal or wait, the head is empty, the tail is reversed into the head.
   This way both signal and wait attempt O(1) and O(n) operations at the same
   time. *)

let rec signal (t : t) backoff =
  match Atomic.get t with
  | T Zero -> ()
  | T (One _ as q) as before ->
      let after = Q.tail q in
      if Atomic.compare_and_set t before after then
        let trigger = Q.head q in
        Trigger.signal trigger
      else signal t (Backoff.once backoff)

let rec cleanup backoff trigger (t : t) =
  (* We have been canceled.  If we can't drop our trigger from the variable, we
     signal the next trigger in queue to make sure each signal wakes up at least
     one non-canceled waiter if possible. *)
  match Atomic.get t with
  | T Zero -> ()
  | T (One _ as q) as before ->
      let after = Q.remove q trigger in
      if before == after then signal t Backoff.default
      else if not (Atomic.compare_and_set t before after) then
        cleanup (Backoff.once backoff) trigger t

let rec wait (t : t) mutex cons fiber backoff =
  let before = Atomic.get t in
  let after = Q.add_cons before cons in
  if Atomic.compare_and_set t before after then begin
    Mutex.unlock_as (Fiber.Maybe.of_fiber fiber) mutex Backoff.default;
    let trigger = S.value cons in
    let result = Trigger.await trigger in
    let forbid = Fiber.exchange fiber ~forbid:true in
    Mutex.lock_as (Fiber.Maybe.of_fiber fiber) mutex (T Nil) Backoff.default;
    Fiber.set fiber ~forbid;
    match result with
    | None -> ()
    | Some (exn, bt) ->
        cleanup Backoff.default trigger t;
        Printexc.raise_with_backtrace exn bt
  end
  else wait t mutex cons fiber (Backoff.once backoff)

let wait t mutex =
  let fiber = Fiber.current () in
  let trigger = Trigger.create () in
  let cons = S.Cons { value = trigger; next = T Nil } in
  wait t mutex cons fiber Backoff.default

let[@inline] signal t = signal t Backoff.default
