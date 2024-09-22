open Picos

let[@inline never] owner () = raise (Sys_error "Mutex: owner")
let[@inline never] unlocked () = raise (Sys_error "Mutex: unlocked")
let[@inline never] not_owner () = raise (Sys_error "Mutex: not owner")

type _ tdt =
  | Entry : { trigger : Trigger.t; fiber : Fiber.Maybe.t } -> [> `Entry ] tdt
  | Nothing : [> `Nothing ] tdt

type state =
  | Locked
  | Unlocked
  | Queued of { fiber : Fiber.Maybe.t; waiters : [ `Entry ] tdt Q.t }

type t = state Atomic.t

let create ?padded () = Multicore_magic.copy_as ?padded @@ Atomic.make Unlocked

let rec unlock_as owner t backoff =
  match Atomic.get t with
  | Unlocked -> unlocked ()
  | Locked as before ->
      if not (Atomic.compare_and_set t before Unlocked) then
        unlock_as owner t (Backoff.once backoff)
  | Queued r as before ->
      if Fiber.Maybe.equal r.fiber owner then
        match r.waiters with
        | T Zero ->
            if not (Atomic.compare_and_set t before Unlocked) then
              unlock_as owner t (Backoff.once backoff)
        | T (One _ as q) ->
            let (Entry { trigger; fiber }) = Q.head q in
            let waiters = Q.tail q in
            let after = Queued { fiber; waiters } in
            if Atomic.compare_and_set t before after then Trigger.signal trigger
            else unlock_as owner t (Backoff.once backoff)
      else not_owner ()

let[@inline] unlock ?checked t =
  match checked with
  | Some false ->
      if
        (* The unlock operation will mutate the atomic location and will be
           sequentially consistent.  The fenceless get potentially allows us to
           avoid performing a failed mutation attempt causing cache coherency
           traffic and fenceless get here performs better on ARM. *)
        Multicore_magic.fenceless_get t != Locked
        || not (Atomic.compare_and_set t Locked Unlocked)
      then unlock_as Fiber.Maybe.nothing t Backoff.default
  | None | Some true ->
      let owner = Fiber.Maybe.of_fiber (Fiber.current ()) in
      unlock_as owner t Backoff.default

let rec cleanup_as (Entry entry_r as entry : [ `Entry ] tdt) t backoff =
  (* We have been canceled.  If we are the owner, we must unlock the mutex.
     Otherwise we must remove our entry from the queue. *)
  match Atomic.get t with
  | Queued r as before -> begin
      match r.waiters with
      | T Zero -> unlock_as entry_r.fiber t backoff
      | T (One _ as q) ->
          let waiters = Q.remove q entry in
          if r.waiters == waiters then unlock_as entry_r.fiber t backoff
          else
            let after = Queued { fiber = r.fiber; waiters } in
            if not (Atomic.compare_and_set t before after) then
              cleanup_as entry t (Backoff.once backoff)
    end
  | Locked -> unlock_as entry_r.fiber t backoff
  | Unlocked -> unlocked ()

let rec lock_as fiber t entry backoff =
  match Atomic.get t with
  | Unlocked as before ->
      let after =
        if fiber == Fiber.Maybe.nothing then Locked
        else Queued { fiber; waiters = T Zero }
      in
      if not (Atomic.compare_and_set t before after) then
        lock_as fiber t entry (Backoff.once backoff)
  | Locked as before ->
      let (Entry entry_r as entry : [ `Entry ] tdt) =
        match entry with
        | Nothing ->
            let trigger = Trigger.create () in
            Entry { trigger; fiber }
        | Entry _ as entry -> entry
      in
      let waiters = Q.singleton entry in
      let after = Queued { fiber = Fiber.Maybe.nothing; waiters } in
      if Atomic.compare_and_set t before after then begin
        match Trigger.await entry_r.trigger with
        | None -> ()
        | Some (exn, bt) ->
            cleanup_as entry t Backoff.default;
            Printexc.raise_with_backtrace exn bt
      end
      else lock_as fiber t entry (Backoff.once backoff)
  | Queued r as before ->
      if Fiber.Maybe.unequal r.fiber fiber then
        let (Entry entry_r as entry : [ `Entry ] tdt) =
          match entry with
          | Nothing ->
              let trigger = Trigger.create () in
              Entry { trigger; fiber }
          | Entry _ as entry -> entry
        in
        let waiters = Q.add r.waiters entry in
        let after = Queued { fiber = r.fiber; waiters } in
        if Atomic.compare_and_set t before after then begin
          match Trigger.await entry_r.trigger with
          | None -> ()
          | Some (exn, bt) ->
              cleanup_as entry t Backoff.default;
              Printexc.raise_with_backtrace exn bt
        end
        else lock_as fiber t entry (Backoff.once backoff)
      else owner ()

let[@inline] lock ?checked t =
  match checked with
  | Some false ->
      if
        (* The lock operation will mutate the atomic location and will be
           sequentially consistent.  The fenceless get potentially allows us to
           avoid performing a failed mutation attempt causing cache coherency
           traffic and fenceless get here performs better on ARM. *)
        Multicore_magic.fenceless_get t != Unlocked
        || not (Atomic.compare_and_set t Unlocked Locked)
      then lock_as Fiber.Maybe.nothing t Nothing Backoff.default
  | None | Some true ->
      let fiber = Fiber.current () in
      Fiber.check fiber;
      lock_as (Fiber.Maybe.of_fiber fiber) t Nothing Backoff.default

let try_lock ?checked t =
  let fiber = Fiber.Maybe.current_and_check_if checked in
  Atomic.get t == Unlocked
  && Atomic.compare_and_set t Unlocked
       (if fiber == Fiber.Maybe.nothing then Locked
        else Queued { fiber; waiters = T Zero })

let protect ?checked t body =
  let fiber = Fiber.Maybe.current_and_check_if checked in
  lock_as fiber t Nothing Backoff.default;
  match body () with
  | value ->
      unlock_as fiber t Backoff.default;
      value
  | exception exn ->
      let bt = Printexc.get_raw_backtrace () in
      unlock_as fiber t Backoff.default;
      Printexc.raise_with_backtrace exn bt
