open Picos

type 'a finally = ('a -> unit) * (unit -> 'a)

let[@inline] finally release acquire = (release, acquire)

(** This function is marked [@inline never] to ensure that there are no
    allocations between the [acquire ()] and the [match ... with] nor before
    [release].  Allocations here would mean that e.g. pressing Ctrl-C, i.e.
    [SIGINT], at the right moment could mean that [release] would not be called
    after [acquire]. *)
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

type ('a, _) tdt =
  | Nothing : ('a, [> `Nothing ]) tdt
  | Resource : {
      mutable resource : 'a;
      release : 'a -> unit;
      moved : Trigger.t;
    }
      -> ('a, [> `Resource ]) tdt

type 'a moveable = ('a, [ `Nothing | `Resource ]) tdt Atomic.t

let ( let^ ) (release, acquire) body =
  let moveable = Atomic.make Nothing in
  let acquire () =
    let (Resource r as state : (_, [ `Resource ]) tdt) =
      Resource { resource = Obj.magic (); release; moved = Trigger.create () }
    in
    r.resource <- acquire ();
    Atomic.set moveable state;
    moveable
  in
  let release moveable =
    match Atomic.get moveable with
    | Nothing -> ()
    | Resource r -> begin
        match Trigger.await r.moved with
        | None -> ()
        | Some exn_bt -> begin
            match Atomic.exchange moveable Nothing with
            | Nothing -> ()
            | Resource r ->
                r.release r.resource;
                Exn_bt.raise exn_bt
          end
      end
  in
  let_at acquire body release

let[@inline never] check_no_resource () =
  (* In case of cancelation this is not considered an error as the resource was
     (likely) released by the parent. *)
  Fiber.check (Fiber.current ());
  invalid_arg "no resource to move"

let nothing = (ignore, check_no_resource)

let move moveable =
  match Atomic.get moveable with
  | Nothing -> nothing
  | Resource r ->
      let acquire () =
        match Atomic.exchange moveable Nothing with
        | Nothing -> check_no_resource ()
        | Resource r ->
            Trigger.signal r.moved;
            r.resource
      in
      (r.release, acquire)
