open Picos

let[@inline never] overflow () = raise (Sys_error "overflow")
let[@inline never] negative () = invalid_arg "negative initial count"

module Counting = struct
  type t = Obj.t Atomic.t

  let make ?padded count =
    if count < 0 then negative ();
    Atomic.make (Obj.repr count) |> Multicore_magic.copy_as ?padded

  let rec release t backoff =
    (* The release operation will mutate the atomic location and will be
       sequentially consistent.  The fenceless get here performs better on
       ARM. *)
    let before = Multicore_magic.fenceless_get t in
    if Obj.is_int before then begin
      let count = Obj.obj before in
      if count < count + 1 then begin
        let after = Obj.repr (count + 1) in
        if not (Atomic.compare_and_set t before after) then
          release t (Backoff.once backoff)
      end
      else overflow ()
    end
    else
      let after = Q.tail (Obj.obj before) in
      if Atomic.compare_and_set t before (Obj.repr after) then
        let trigger = Q.head (Obj.obj before) in
        Trigger.signal trigger
      else release t (Backoff.once backoff)

  let rec cleanup t trigger backoff =
    let before = Atomic.get t in
    if Obj.is_int before then release t Backoff.default
    else
      let before = Obj.obj before in
      let after = Q.remove before trigger in
      if before == after then release t Backoff.default
      else if not (Atomic.compare_and_set t (Obj.repr before) (Obj.repr after))
      then cleanup t trigger (Backoff.once backoff)

  let rec acquire t backoff =
    (* The acquire operation will mutate the atomic location and will be
       sequentially consistent.  The fenceless get here performs better on
       ARM. *)
    let before = Multicore_magic.fenceless_get t in
    if Obj.is_int before then
      let count = Obj.obj before in
      if 0 < count then begin
        let after = Obj.repr (count - 1) in
        if not (Atomic.compare_and_set t before after) then
          acquire t (Backoff.once backoff)
      end
      else
        let trigger = Trigger.create () in
        let after = Q.singleton trigger in
        if Atomic.compare_and_set t before (Obj.repr after) then begin
          match Trigger.await trigger with
          | None -> ()
          | Some (exn, bt) ->
              cleanup t trigger Backoff.default;
              Printexc.raise_with_backtrace exn bt
        end
        else acquire t (Backoff.once backoff)
    else
      let trigger = Trigger.create () in
      let after = Q.snoc (Obj.obj before) trigger in
      if Atomic.compare_and_set t before (Obj.repr after) then begin
        match Trigger.await trigger with
        | None -> ()
        | Some (exn, bt) ->
            cleanup t trigger Backoff.default;
            Printexc.raise_with_backtrace exn bt
      end
      else acquire t (Backoff.once backoff)

  let rec try_acquire t backoff =
    let before = Atomic.get t in
    Obj.is_int before
    &&
    let count = Obj.obj before in
    0 < count
    &&
    let after = Obj.repr (count - 1) in
    Atomic.compare_and_set t before after
    || try_acquire t (Backoff.once backoff)

  let get_value t =
    let state = Atomic.get t in
    if Obj.is_int state then Obj.obj state else 0

  let[@inline] release t = release t Backoff.default
  let[@inline] acquire t = acquire t Backoff.default
  let[@inline] try_acquire t = try_acquire t Backoff.default
end

module Binary = struct
  type t = Counting.t

  let make ?padded initial = Counting.make ?padded (Bool.to_int initial)

  let rec release t backoff =
    (* The release operation will mutate the atomic location and will be
       sequentially consistent.  The fenceless get here performs better on
       ARM. *)
    let before = Multicore_magic.fenceless_get t in
    if Obj.is_int before then begin
      let count = Obj.obj before in
      if count = 0 then
        let after = Obj.repr 1 in
        if not (Atomic.compare_and_set t before after) then
          release t (Backoff.once backoff)
    end
    else
      let after = Q.tail (Obj.obj before) in
      if Atomic.compare_and_set t before (Obj.repr after) then
        let trigger = Q.head (Obj.obj before) in
        Trigger.signal trigger
      else release t (Backoff.once backoff)

  let acquire = Counting.acquire
  let try_acquire = Counting.try_acquire
  let[@inline] release t = release t Backoff.default
end
