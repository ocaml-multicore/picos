open Picos

let[@inline never] completed () = invalid_arg "already completed"

type _ tdt =
  | Nothing : [> `Nothing ] tdt
  | Bundle : {
      num_fibers : int Atomic.t;
      bundle : unit Computation.t;
      errors : Control.Errors.t;
      finished : Trigger.t;
    }
      -> [> `Bundle ] tdt

let flock_key : [ `Bundle | `Nothing ] tdt Fiber.FLS.key =
  Fiber.FLS.new_key (Constant Nothing)

type t = [ `Bundle ] tdt

let terminate ?callstack (Bundle r : t) =
  Computation.cancel r.bundle (Control.terminate_bt ?callstack ())

let terminate_after ?callstack (Bundle r : t) ~seconds =
  Computation.cancel_after r.bundle ~seconds
    (Control.terminate_bt ?callstack ())

let error ?callstack (Bundle r as t : t) (exn_bt : Exn_bt.t) =
  if exn_bt.Exn_bt.exn != Control.Terminate then begin
    terminate ?callstack t;
    Control.Errors.push r.errors exn_bt
  end

let decr (Bundle r : t) =
  let n = Atomic.fetch_and_add r.num_fibers (-1) in
  if n = 1 then begin
    Computation.finish r.bundle;
    Trigger.signal r.finished
  end

type _ pass = FLS : unit pass | Arg : t pass

let[@inline never] no_flock () = invalid_arg "no flock"

let get_flock fiber =
  match Fiber.FLS.get fiber flock_key with
  | Nothing -> no_flock ()
  | Bundle _ as t -> t

let await (type a) (Bundle r as t : t) fiber packed canceler outer
    (pass : a pass) =
  decr t;
  Fiber.set_computation fiber packed;
  let forbid = Fiber.exchange fiber ~forbid:true in
  Trigger.await r.finished |> ignore;
  Fiber.set fiber ~forbid;
  begin
    match pass with FLS -> Fiber.FLS.set fiber flock_key outer | Arg -> ()
  end;
  let (Packed parent) = packed in
  Computation.detach parent canceler;
  Control.Errors.check r.errors;
  Fiber.check fiber

let join_after_pass (type a) (fn : a -> _) (pass : a pass) =
  (* The sequence of operations below ensures that nothing is leaked. *)
  let (Bundle r as t : t) =
    let num_fibers = Atomic.make 1 in
    let bundle = Computation.create ~mode:`LIFO () in
    let errors = Control.Errors.create () in
    let finished = Trigger.create () in
    Bundle { num_fibers; bundle; errors; finished }
  in
  let fiber = Fiber.current () in
  let outer =
    match pass with FLS -> Fiber.FLS.get fiber flock_key | Arg -> Nothing
  in
  let (Packed parent as packed) = Fiber.get_computation fiber in
  let bundle = Computation.Packed r.bundle in
  let canceler = Computation.attach_canceler ~from:parent ~into:r.bundle in
  (* Ideally there should be no poll point betweem [attach_canceler] and the
     [match ... with] below. *)
  match
    Fiber.set_computation fiber bundle;
    fn (match pass with FLS -> Fiber.FLS.set fiber flock_key t | Arg -> t)
  with
  | value ->
      await t fiber packed canceler outer pass;
      value
  | exception exn ->
      let exn_bt = Exn_bt.get exn in
      error t exn_bt;
      await t fiber packed canceler outer pass;
      Exn_bt.raise exn_bt

let rec incr (Bundle r as t : t) backoff =
  let before = Atomic.get r.num_fibers in
  if before = 0 then completed ()
  else if not (Atomic.compare_and_set r.num_fibers before (before + 1)) then
    incr t (Backoff.once backoff)

let fork_as_promise_pass (type a) (Bundle r as t : t) thunk (pass : a pass) =
  (* The sequence of operations below ensures that nothing is leaked. *)
  incr t Backoff.default;
  try
    let child = Computation.create ~mode:`LIFO () in
    let canceler = Computation.attach_canceler ~from:r.bundle ~into:child in
    let main =
      match pass with
      | FLS ->
          let main () =
            let fiber = Fiber.current () in
            Fiber.FLS.set fiber flock_key t;
            begin
              match thunk () with
              | value -> Computation.return child value
              | exception exn ->
                  let exn_bt = Exn_bt.get exn in
                  Computation.cancel child exn_bt;
                  error (get_flock fiber) exn_bt
            end;
            let (Bundle r as t : t) = get_flock fiber in
            Computation.detach r.bundle canceler;
            decr t
          in
          [ main ]
      | Arg ->
          let main () =
            begin
              match thunk () with
              | value -> Computation.return child value
              | exception exn ->
                  let exn_bt = Exn_bt.get exn in
                  Computation.cancel child exn_bt;
                  error t exn_bt
            end;
            Computation.detach r.bundle canceler;
            decr t
          in
          [ main ]
    in
    Fiber.spawn ~forbid:false child main;
    child
  with canceled_exn ->
    (* We don't worry about detaching the [canceler], because at this point we
       know the bundle computation has completed or there is something more
       serious. *)
    decr t;
    raise canceled_exn

let fork_pass (type a) (Bundle r as t : t) thunk (pass : a pass) =
  (* The sequence of operations below ensures that nothing is leaked. *)
  incr t Backoff.default;
  try
    let main =
      match pass with
      | FLS ->
          let main () =
            let fiber = Fiber.current () in
            Fiber.FLS.set fiber flock_key t;
            begin
              try thunk ()
              with exn -> error (get_flock fiber) (Exn_bt.get exn)
            end;
            decr (get_flock fiber)
          in
          [ main ]
      | Arg ->
          let main () =
            begin
              try thunk () with exn -> error t (Exn_bt.get exn)
            end;
            decr t
          in
          [ main ]
    in
    Fiber.spawn ~forbid:false r.bundle main
  with canceled_exn ->
    decr t;
    raise canceled_exn

(* *)

let join_after fn = join_after_pass fn Arg
let fork t thunk = fork_pass t thunk Arg
let fork_as_promise t thunk = fork_as_promise_pass t thunk Arg
let is_running (Bundle r : t) = Computation.is_running r.bundle
let unsafe_incr (Bundle r : t) = Atomic.incr r.num_fibers
let unsafe_reset (Bundle r : t) = Atomic.set r.num_fibers 1
