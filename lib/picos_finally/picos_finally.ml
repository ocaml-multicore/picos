open Picos

type 'a template = { release : 'a -> unit; acquire : unit -> 'a }

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

let rec release instance =
  match Atomic.get instance with
  | Moved | Released -> ()
  | Borrowed as case -> error case
  | Resource r as before ->
      if Atomic.compare_and_set instance before Released then begin
        r.release r.resource;
        Trigger.signal r.moved_or_released
      end
      else release instance

let await_moved_or_released instance =
  match Atomic.get instance with
  | Moved | Released -> ()
  | Borrowed as case ->
      (* This should be impossible as [let&] should have restored the state. *)
      error case
  | Resource r -> begin
      match Trigger.await r.moved_or_released with
      | None ->
          (* We release in case we could not wait. *)
          release instance
      | Some exn_bt ->
          (* We have been canceled, so we release. *)
          release instance;
          Exn_bt.raise exn_bt
    end

(** This function is marked [@inline never] to ensure that there are no
    allocations between the [acquire ()] and the [match ... with] nor before
    [release]. *)
let[@inline never] let_at acquire body ~on_return ~on_raise =
  let x = acquire () in
  match body x with
  | y ->
      on_return x;
      y
  | exception exn ->
      on_raise x;
      raise exn

let ( let^ ) t body =
  let moved_or_released = Trigger.create () in
  let state =
    Resource { resource = Obj.magic (); release = t.release; moved_or_released }
  in
  let instance = Atomic.make state in
  (* [acquire] is called once by [let_at] before [instance] is returned, which
     means the [Obj.magic] use below is safe. *)
  let acquire () =
    let (Resource r : (_, [ `Resource ]) tdt) =
      Obj.magic (Atomic.get instance)
    in
    r.resource <- t.acquire ();
    instance
  in
  let_at acquire body ~on_return:await_moved_or_released ~on_raise:release

let rec ( let& ) instance body =
  match Atomic.get instance with
  | (Moved | Released | Borrowed) as case -> error case
  | Resource r as before ->
      if Atomic.compare_and_set instance before Borrowed then begin
        match body r.resource with
        | result ->
            Atomic.set instance before;
            result
        | exception exn ->
            Atomic.set instance before;
            raise exn
      end
      else ( let& ) instance body

let move instance =
  match Atomic.get instance with
  | (Moved | Borrowed) as case -> error case
  | Released -> { release = ignore; acquire = check_released }
  | Resource r ->
      let rec acquire () =
        match Atomic.get instance with
        | (Moved | Borrowed) as case -> error case
        | Released -> check_released ()
        | Resource r as before ->
            if Atomic.compare_and_set instance before Moved then begin
              (* [Trigger.signal] should never raise exceptions. *)
              Trigger.signal r.moved_or_released;
              r.resource
            end
            else acquire ()
      in
      { release = r.release; acquire }

let[@inline] ( let@ ) t body =
  let_at t.acquire body ~on_return:t.release ~on_raise:t.release

let[@inline] finally release acquire = { release; acquire }
