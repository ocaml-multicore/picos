open Picos

type t = {
  num_fibers : int Atomic.t;
  bundle : unit Computation.t;
  errors : Control.Errors.t;
  finished : Trigger.t;
}

let terminate ?callstack t =
  let terminate_bt = Control.terminate_bt ?callstack () in
  Computation.cancel t.bundle terminate_bt

let error t exn_bt =
  terminate t;
  Control.Errors.push t.errors exn_bt

let decr t =
  let n = Atomic.fetch_and_add t.num_fibers (-1) in
  if n = 1 then begin
    Computation.finish t.bundle;
    Trigger.signal t.finished
  end

let await t fiber packed canceler =
  decr t;
  Fiber.set_computation fiber packed;
  let forbid = Fiber.exchange fiber ~forbid:true in
  Trigger.await t.finished |> ignore;
  Fiber.set fiber ~forbid;
  let (Packed parent) = packed in
  Computation.detach parent canceler;
  Control.Errors.check t.errors;
  Fiber.check fiber

let join_after fn =
  let t =
    let num_fibers = Atomic.make 1 in
    let bundle = Computation.create () in
    let errors = Control.Errors.create () in
    let finished = Trigger.create () in
    { num_fibers; bundle; errors; finished }
  in
  let fiber = Fiber.current () in
  let (Packed parent as packed) = Fiber.get_computation fiber in
  let bundle = Computation.Packed t.bundle in
  let canceler = Computation.attach_canceler ~from:parent ~into:t.bundle in
  (* TODO: Ideally there should be no poll point betweem [attach_canceler] and
     the [match ... with] below. *)
  Fiber.set_computation fiber bundle;
  match fn t with
  | value ->
      await t fiber packed canceler;
      value
  | exception exn ->
      let exn_bt = Exn_bt.get exn in
      error t exn_bt;
      await t fiber packed canceler;
      Exn_bt.raise exn_bt

let[@inline never] completed () = invalid_arg "already completed"

let rec incr t backoff =
  let before = Atomic.get t.num_fibers in
  if before = 0 then completed ()
  else if not (Atomic.compare_and_set t.num_fibers before (before + 1)) then
    incr t (Backoff.once backoff)

let fork t thunk =
  incr t Backoff.default;
  try
    let child = Computation.create () in
    let canceler = Computation.attach_canceler ~from:t.bundle ~into:child in
    let main () =
      begin
        match thunk () with
        | () -> Computation.finish child
        | exception exn ->
            let exn_bt = Exn_bt.get exn in
            Computation.cancel child exn_bt;
            error t exn_bt
      end;
      Computation.detach t.bundle canceler;
      decr t
    in
    Fiber.spawn ~forbid:false child [ main ]
  with canceled_exn ->
    decr t;
    raise canceled_exn
