open Picos

type ('a, _) tdt =
  | Transferred : ('a, [> `Transferred ]) tdt
  | Borrowed : ('a, [> `Borrowed ]) tdt
  | Dropped : ('a, [> `Dropped ]) tdt
  | Resource : {
      mutable resource : 'a;
      release : 'a -> unit;
      transferred_or_dropped : Trigger.t;
    }
      -> ('a, [> `Resource ]) tdt

type 'a instance =
  ('a, [ `Transferred | `Borrowed | `Dropped | `Resource ]) tdt Atomic.t

(* *)

let[@inline never] error
    (case : (_, [< `Transferred | `Borrowed | `Dropped ]) tdt) =
  invalid_arg
    (match case with
    | Transferred -> "transferred"
    | Dropped -> "dropped"
    | Borrowed -> "borrowed")

let[@inline never] check_released () =
  (* In case of cancelation we do not consider being released an error as the
     resource was released by (the |an)other party involved in the [move]. *)
  Fiber.check (Fiber.current ());
  error Dropped

(* *)

let forbidden release x =
  match Fiber.current () with
  | fiber -> begin
      if Fiber.exchange fiber ~forbid:true then release x
      else
        match release x with
        | () -> Fiber.set fiber ~forbid:false
        | exception exn ->
            Fiber.set fiber ~forbid:false;
            raise exn
    end
  | exception _exn ->
      (* This should only happen when not running under a scheduler.  However,
         we don't match on a specific exception, because it depends on the OCaml
         version.

         We also do not reraise the exception! *)
      release x

let[@inline never] release_and_reraise exn x release =
  let bt = Printexc.get_raw_backtrace () in
  forbidden release x;
  Printexc.raise_with_backtrace exn bt

let[@inline never] release_and_return value x release =
  forbidden release x;
  value

(* *)

let rec drop instance =
  match Atomic.get instance with
  | Transferred | Dropped -> ()
  | Borrowed as case -> error case
  | Resource r as before ->
      if Atomic.compare_and_set instance before Dropped then begin
        forbidden r.release r.resource;
        Trigger.signal r.transferred_or_dropped
      end
      else drop instance

let[@inline never] drop_and_reraise_as bt instance exn =
  drop instance;
  Printexc.raise_with_backtrace exn bt

let[@inline never] drop_and_reraise exn instance =
  let bt = Printexc.get_raw_backtrace () in
  drop_and_reraise_as bt instance exn

(* *)

let await_transferred_or_dropped instance result =
  match Atomic.get instance with
  | Transferred | Dropped -> result
  | Borrowed as case ->
      (* This should be impossible as [let@ _ = borrow _ in _] should have
         restored the state. *)
      error case
  | Resource r -> begin
      match Trigger.await r.transferred_or_dropped with
      | None ->
          (* We release in case we could not wait. *)
          drop instance;
          result
      | Some (exn, bt) ->
          (* We have been canceled, so we release. *)
          drop_and_reraise_as bt instance exn
    end

let[@inline never] instantiate instance scope =
  match scope instance with
  | result -> await_transferred_or_dropped instance result
  | exception exn -> drop_and_reraise exn instance

let[@inline never] instantiate release acquire scope =
  let instance =
    Sys.opaque_identity
      begin
        let transferred_or_dropped = Trigger.create () in
        let state =
          Resource { resource = Obj.magic (); release; transferred_or_dropped }
        in
        Atomic.make state
      end
  in
  (* After this point there must be no allocations before [acquire ()]. *)
  let (Resource r : (_, [ `Resource ]) tdt) = Obj.magic (Atomic.get instance) in
  r.resource <- acquire ();
  instantiate instance scope

(* *)

let[@inline never] rec transfer from scope =
  match Atomic.get from with
  | (Transferred | Borrowed) as case -> error case
  | Dropped -> check_released ()
  | Resource r as before ->
      let into = Atomic.make Transferred in
      if Atomic.compare_and_set from before Transferred then begin
        Atomic.set into before;
        match
          Trigger.signal r.transferred_or_dropped;
          scope into
        with
        | result -> await_transferred_or_dropped into result
        | exception exn -> drop_and_reraise exn into
      end
      else transfer from scope

(* *)

let[@inline never] rec borrow instance scope =
  match Atomic.get instance with
  | (Transferred | Dropped | Borrowed) as case -> error case
  | Resource r as before ->
      if Atomic.compare_and_set instance before Borrowed then begin
        match scope r.resource with
        | result ->
            Atomic.set instance before;
            result
        | exception exn ->
            (* [Atomic.set] should not disturb the stack trace. *)
            Atomic.set instance before;
            raise exn
      end
      else borrow instance scope

(* *)

let[@inline never] rec move from scope =
  match Atomic.get from with
  | (Transferred | Borrowed) as case -> error case
  | Dropped -> check_released ()
  | Resource r as before ->
      if Atomic.compare_and_set from before Transferred then begin
        match
          Trigger.signal r.transferred_or_dropped;
          scope r.resource
        with
        | result ->
            forbidden r.release r.resource;
            result
        | exception exn -> release_and_reraise exn r.resource r.release
      end
      else move from scope

(* *)

let[@inline never] finally x scope release =
  match scope x with
  | y -> release_and_return y x release
  | exception exn -> release_and_reraise exn x release

let[@inline never] finally release acquire scope =
  let x = acquire () in
  finally x scope release

let[@inline never] lastly action scope =
  match scope () with
  | value -> release_and_return value () action
  | exception exn -> release_and_reraise exn () action

external ( let@ ) : ('a -> 'b) -> 'a -> 'b = "%apply"
