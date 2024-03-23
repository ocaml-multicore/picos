open Picos

type 'a state =
  | Returned of 'a
  | Raised of Exn_bt.t
  | Initial of (unit -> 'a)
  | Running of {
      fiber : Fiber.t;
          (** The [fiber] is for checking against recursive [force] calls. *)
      computation : unit Computation.t;
          (** A [computation] is used to maintain a FIFO of awaiters. *)
    }

type 'a t = 'a state Atomic.t

let from_fun thunk = Atomic.make (Initial thunk)
let from_val value = Atomic.make (Returned value)

let rec force t =
  match Atomic.get t with
  | Returned value -> value
  | Raised exn_bt -> Exn_bt.raise exn_bt
  | Initial thunk as before ->
      let fiber = Fiber.current () in
      let computation = Computation.create () in
      let canceler = Fiber.attach_as_child fiber computation in
      let after = Running { fiber; computation } in
      if Atomic.compare_and_set t before after then begin
        Atomic.set t
          (match thunk () with
          | value -> Returned value
          | exception exn -> Raised (Exn_bt.get exn));
        Computation.finish computation
      end;
      Fiber.detach fiber canceler;
      force t
  | Running r ->
      if Fiber.equal r.fiber (Fiber.current ()) then raise Stdlib.Lazy.Undefined
      else begin
        Computation.wait r.computation;
        force t
      end

let map f t = from_fun @@ fun () -> f (force t)
