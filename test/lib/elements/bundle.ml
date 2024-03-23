open Picos

(* TODO: propagation of all exceptions? *)
(* TODO: cancelation forbidden? *)

type t = { num_fibers : int Atomic.t; computation : unit Computation.t }

let decr t =
  let n = Atomic.fetch_and_add t.num_fibers (-1) in
  if n = 1 then Computation.finish t.computation

let run fn =
  let t =
    let num_fibers = Atomic.make 1 in
    let computation = Computation.create () in
    { num_fibers; computation }
  in
  let fiber = Fiber.current () in
  let canceler = Fiber.attach_as_child fiber t.computation in
  match fn t with
  | value ->
      decr t;
      Computation.await t.computation;
      Fiber.detach fiber canceler;
      value
  | exception exn ->
      let exn_bt = Exn_bt.get exn in
      Computation.cancel t.computation exn_bt;
      Fiber.detach fiber canceler;
      Exn_bt.raise exn_bt

let fork t thunk =
  let child = Computation.create () in
  let canceler = Computation.attach_canceler ~from:t.computation ~into:child in
  Atomic.incr t.num_fibers;
  match
    let main () =
      Computation.capture child thunk ();
      Computation.detach t.computation canceler;
      decr t
    in
    Fiber.spawn ~forbid:false child [ main ]
  with
  | () -> Promise.of_computation child
  | exception canceled_exn ->
      decr t;
      raise canceled_exn
