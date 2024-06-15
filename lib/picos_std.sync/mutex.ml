open Picos

let[@inline never] owner () = raise (Sys_error "Mutex: owner")
let[@inline never] unlocked () = raise (Sys_error "Mutex: unlocked")
let[@inline never] not_owner () = raise (Sys_error "Mutex: not owner")

type entry = { trigger : Trigger.t; fiber : Fiber.Maybe.t }

type _ tdt =
  | Locked : [> `Locked ] tdt
  | Unlocked : [> `Unlocked ] tdt
  | Locked_by : { fiber : Fiber.Maybe.t } -> [> `Locked_by ] tdt
  | Contended : {
      fiber : Fiber.Maybe.t;
      head : entry S.cons;
      tail : entry S.cons;
      cons : entry S.cons;
    }
      -> [> `Contended ] tdt

type state =
  | T : [< `Locked | `Unlocked | `Locked_by | `Contended ] tdt -> state
[@@unboxed]

type t = state Atomic.t

let create ?padded () =
  Multicore_magic.copy_as ?padded @@ Atomic.make (T Unlocked)

let rec unlock_as owner t backoff =
  match Atomic.get t with
  | T Unlocked -> unlocked ()
  | T Locked as before ->
      if not (Atomic.compare_and_set t before (T Unlocked)) then
        unlock_as owner t (Backoff.once backoff)
  | T (Locked_by r) as before ->
      if Fiber.Maybe.equal r.fiber owner then begin
        if not (Atomic.compare_and_set t before (T Unlocked)) then
          unlock_as owner t (Backoff.once backoff)
      end
      else not_owner ()
  | T (Contended r) as before ->
      if Fiber.Maybe.equal r.fiber owner then begin
        S.exec r.tail r.cons;
        let { trigger; fiber } = S.value r.head in
        let after =
          if r.head != r.cons then
            let head = S.next_as_cons r.head in
            Contended { fiber; head; tail = r.cons; cons = r.cons }
          else if fiber == Fiber.Maybe.nothing then Locked
          else Locked_by { fiber }
        in
        if Atomic.compare_and_set t before (T after) then Trigger.signal trigger
        else unlock_as owner t (Backoff.once backoff)
      end
      else not_owner ()

let[@inline] unlock ?checked t =
  match checked with
  | Some false ->
      if
        (* The unlock operation will mutate the atomic location and will be
           sequentially consistent.  The fenceless get potentially allows us to
           avoid performing a failed mutation attempt causing cache coherency
           traffic and fenceless get here performs better on ARM. *)
        Multicore_magic.fenceless_get t != T Locked
        || not (Atomic.compare_and_set t (T Locked) (T Unlocked))
      then unlock_as Fiber.Maybe.nothing t Backoff.default
  | None | Some true ->
      let owner = Fiber.Maybe.of_fiber (Fiber.current ()) in
      unlock_as owner t Backoff.default

let rec cleanup_as entry t backoff =
  (* We have been canceled.  If we are the owner, we must unlock the mutex.
     Otherwise we must remove our entry from the queue. *)
  match Atomic.get t with
  | T (Contended r) as before -> begin
      S.exec r.tail r.cons;
      match S.reject r.head entry with
      | S.T Nil ->
          let after = Locked_by { fiber = r.fiber } in
          if not (Atomic.compare_and_set t before (T after)) then
            cleanup_as entry t (Backoff.once backoff)
      | S.T (Cons _ as head) ->
          let tail = S.find_tail head in
          let after = Contended { fiber = r.fiber; head; tail; cons = tail } in
          if not (Atomic.compare_and_set t before (T after)) then
            cleanup_as entry t (Backoff.once backoff)
      | exception Not_found -> unlock_as entry.fiber t backoff
    end
  | T Locked | T (Locked_by _) -> unlock_as entry.fiber t backoff
  | T Unlocked -> unlocked ()

let rec lock_as fiber t node backoff =
  match Atomic.get t with
  | T Unlocked as before ->
      let after =
        if fiber == Fiber.Maybe.nothing then Locked else Locked_by { fiber }
      in
      if not (Atomic.compare_and_set t before (T after)) then
        lock_as fiber t node (Backoff.once backoff)
  | T ((Locked | Locked_by _ | Contended _) as other) ->
      let cons =
        match node with
        | S.T Nil ->
            let trigger = Trigger.create () in
            let value = { trigger; fiber } in
            S.Cons { value; next = T Nil }
        | S.T (Cons _ as cons) -> cons
      in
      let after =
        match (other : [ `Locked | `Locked_by | `Contended ] tdt) with
        | Locked ->
            Contended
              { fiber = Fiber.Maybe.nothing; head = cons; tail = cons; cons }
        | Locked_by r ->
            if Fiber.Maybe.unequal r.fiber fiber then
              Contended { fiber = r.fiber; head = cons; tail = cons; cons }
            else owner ()
        | Contended r ->
            if Fiber.Maybe.unequal r.fiber fiber then begin
              S.exec r.tail r.cons;
              Contended { r with tail = r.cons; cons }
            end
            else owner ()
      in
      if Atomic.compare_and_set t (T other) (T after) then begin
        let entry = S.value cons in
        match Trigger.await entry.trigger with
        | None -> ()
        | Some (exn, bt) ->
            cleanup_as entry t Backoff.default;
            Printexc.raise_with_backtrace exn bt
      end
      else lock_as fiber t (T cons) (Backoff.once backoff)

let[@inline] lock ?checked t =
  match checked with
  | Some false ->
      if
        (* The lock operation will mutate the atomic location and will be
           sequentially consistent.  The fenceless get potentially allows us to
           avoid performing a failed mutation attempt causing cache coherency
           traffic and fenceless get here performs better on ARM. *)
        Multicore_magic.fenceless_get t != T Unlocked
        || not (Atomic.compare_and_set t (T Unlocked) (T Locked))
      then lock_as Fiber.Maybe.nothing t (T Nil) Backoff.default
  | None | Some true ->
      let fiber = Fiber.current () in
      Fiber.check fiber;
      lock_as (Fiber.Maybe.of_fiber fiber) t (T Nil) Backoff.default

let try_lock ?checked t =
  let fiber = Fiber.Maybe.current_and_check_if checked in
  Atomic.get t == T Unlocked
  && Atomic.compare_and_set t (T Unlocked)
       (T (if fiber == Fiber.Maybe.nothing then Locked else Locked_by { fiber }))

let protect ?checked t body =
  let fiber = Fiber.Maybe.current_and_check_if checked in
  lock_as fiber t (T Nil) Backoff.default;
  match body () with
  | value ->
      unlock_as fiber t Backoff.default;
      value
  | exception exn ->
      let bt = Printexc.get_raw_backtrace () in
      unlock_as fiber t Backoff.default;
      Printexc.raise_with_backtrace exn bt
