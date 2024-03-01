open Picos

(* TODO: propagation of all exceptions? *)
(* TODO: cancelation forbidden? *)

type t = { num_fibers : int Atomic.t; computation : unit Computation.t }

let decr t =
  let n = Atomic.fetch_and_add t.num_fibers (-1) in
  if n = 1 then Computation.finish t.computation

let run fn =
  let num_fibers = Atomic.make 1 in
  let computation = Computation.create () in
  let t = { num_fibers; computation } in
  let fiber = Fiber.current () in
  let (Packed fiber_computation) = Fiber.computation fiber in
  let canceler =
    Computation.canceler ~from:fiber_computation ~into:t.computation
  in
  if Computation.try_attach fiber_computation canceler then begin
    match fn t with
    | value ->
        decr t;
        Computation.await t.computation;
        Computation.detach fiber_computation canceler;
        value
    | exception exn ->
        let exn_bt = Exn_bt.get exn in
        Computation.cancel t.computation exn_bt;
        Computation.detach fiber_computation canceler;
        Exn_bt.raise exn_bt
  end
  else
    match Computation.canceled fiber_computation with
    | None -> failwith "Bundle: fiber already finished"
    | Some exn_bt -> Exn_bt.raise exn_bt

let fork t thunk =
  let child_computation = Computation.create () in
  let canceler =
    Computation.canceler ~from:t.computation ~into:child_computation
  in
  if Computation.try_attach t.computation canceler then begin
    Atomic.incr t.num_fibers;
    match
      let main () =
        Computation.capture child_computation thunk ();
        Computation.detach t.computation canceler;
        decr t
      in
      Fiber.spawn ~forbid:false child_computation [ main ]
    with
    | () -> Promise.of_computation child_computation
    | exception canceled_exn ->
        decr t;
        raise canceled_exn
  end
  else
    match Computation.canceled t.computation with
    | None -> invalid_arg "Bundle: bundle already finished"
    | Some exn_bt -> Exn_bt.raise exn_bt
