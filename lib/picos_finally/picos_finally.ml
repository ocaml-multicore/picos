open Picos

type 'a template = ('a -> unit) * (unit -> 'a)

type ('a, _) tdt =
  | Moved : ('a, [> `Moved ]) tdt
  | Borrowed : ('a, [> `Borrowed ]) tdt
  | Released : ('a, [> `Released ]) tdt
  | Resource : {
      mutable resource : 'a;
      release : 'a -> unit;
      moved_or_released : Trigger.t;
    }
      -> ('a, [> `Resource ]) tdt

type 'a instance =
  ('a, [ `Moved | `Borrowed | `Released | `Resource ]) tdt Atomic.t

let[@inline never] error (case : (_, [< `Moved | `Borrowed | `Released ]) tdt) =
  invalid_arg
    (match case with
    | Moved -> "moved"
    | Released -> "released"
    | Borrowed -> "borrowed")

let[@inline never] check_released () =
  (* In case of cancelation we do not consider being released an error as the
     resource was released by (the |an)other party involved in the [move]. *)
  Fiber.check (Fiber.current ());
  error Released

let rec release movable =
  match Atomic.get movable with
  | Moved | Released -> ()
  | Borrowed as case -> error case
  | Resource r as before ->
      if Atomic.compare_and_set movable before Released then begin
        r.release r.resource;
        Trigger.signal r.moved_or_released
      end
      else release movable

let await_moved_or_released movable =
  match Atomic.get movable with
  | Moved | Released -> ()
  | Borrowed as case ->
      (* This should be impossible as [let&] should have restored the state. *)
      error case
  | Resource r -> begin
      match Trigger.await r.moved_or_released with
      | None ->
          (* We release in case we could not wait. *)
          release movable
      | Some exn_bt ->
          (* We have been canceled, so we release. *)
          release movable;
          Exn_bt.raise exn_bt
    end

(** This function is marked [@inline never] to ensure that there are no
    allocations between the [acquire ()] and the [match ... with] nor before
    [release]. *)
let[@inline never] let_caret acquire body =
  let x = acquire () in
  match body x with
  | y ->
      await_moved_or_released x;
      y
  | exception exn ->
      release x;
      raise exn

let ( let^ ) (release, acquire) body =
  let moved_or_released = Trigger.create () in
  let state =
    Resource { resource = Obj.magic (); release; moved_or_released }
  in
  let movable = Atomic.make state in
  (* [acquire] is called once by [let_at] before [movable] is returned, which
     means the [Obj.magic] use below is safe. *)
  let acquire () =
    let (Resource r : (_, [ `Resource ]) tdt) =
      Obj.magic (Atomic.get movable)
    in
    r.resource <- acquire ();
    movable
  in
  let_caret acquire body

let rec ( let& ) movable body =
  match Atomic.get movable with
  | (Moved | Released | Borrowed) as case -> error case
  | Resource r as before ->
      if Atomic.compare_and_set movable before Borrowed then begin
        match body r.resource with
        | result ->
            Atomic.set movable before;
            result
        | exception exn ->
            Atomic.set movable before;
            raise exn
      end
      else ( let& ) movable body

let move movable =
  match Atomic.get movable with
  | (Moved | Borrowed) as case -> error case
  | Released -> (ignore, check_released)
  | Resource r ->
      let rec acquire () =
        match Atomic.get movable with
        | (Moved | Borrowed) as case -> error case
        | Released -> check_released ()
        | Resource r as before ->
            if Atomic.compare_and_set movable before Moved then begin
              (* [Trigger.signal] should never raise exceptions. *)
              Trigger.signal r.moved_or_released;
              r.resource
            end
            else acquire ()
      in
      (r.release, acquire)

(** This function is marked [@inline never] to ensure that there are no
    allocations between the [acquire ()] and the [match ... with] nor before
    [release]. *)
let[@inline never] let_at acquire body release =
  let x = acquire () in
  match body x with
  | y ->
      release x;
      y
  | exception exn ->
      release x;
      raise exn

let[@inline] ( let@ ) (release, acquire) body = let_at acquire body release
let[@inline] finally release acquire = (release, acquire)
