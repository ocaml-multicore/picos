open Picos

let[@inline never] completed () = invalid_arg "already completed"

type _ tdt =
  | Nothing : [> `Nothing ] tdt
  | Bundle : {
      config : int Atomic.t;
      bundle : Computation.packed;
      errors : Control.Errors.t;
      finished : Trigger.t;
    }
      -> [> `Bundle ] tdt

let config_terminated_bit = 0x01
and config_callstack_mask = 0x3E
and config_callstack_shift = 1
and config_one = 0x40 (* memory runs out before overflow *)

let flock_key : [ `Bundle | `Nothing ] tdt Fiber.FLS.t = Fiber.FLS.create ()

type t = [ `Bundle ] tdt

let terminate ?callstack (Bundle { bundle = Packed bundle; _ } : t) =
  Computation.cancel bundle Control.Terminate
    (Control.get_callstack_opt callstack)

let terminate_after ?callstack (Bundle { bundle = Packed bundle; _ } : t)
    ~seconds =
  Computation.cancel_after bundle ~seconds Control.Terminate
    (Control.get_callstack_opt callstack)

let error ?callstack (Bundle r as t : t) exn bt =
  if exn != Control.Terminate then begin
    terminate ?callstack t;
    Control.Errors.push r.errors exn bt
  end

let decr (Bundle r : t) =
  let n = Atomic.fetch_and_add r.config (-config_one) in
  if n < config_one * 2 then begin
    let (Packed bundle) = r.bundle in
    Computation.cancel bundle Control.Terminate Control.empty_bt;
    Trigger.signal r.finished
  end

type _ pass = FLS : unit pass | Arg : t pass

let[@inline never] no_flock () = invalid_arg "no flock"

let get_flock fiber =
  match Fiber.FLS.get fiber flock_key ~default:Nothing with
  | Bundle _ as t -> t
  | Nothing -> no_flock ()

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

let join_after_pass (type a) ?callstack ?on_return (fn : a -> _) (pass : a pass)
    =
  (* The sequence of operations below ensures that nothing is leaked. *)
  let (Bundle r as t : t) =
    let terminated =
      match on_return with
      | None | Some `Wait -> 0
      | Some `Terminate -> config_terminated_bit
    in
    let callstack =
      match callstack with
      | None -> 0
      | Some n ->
          if n <= 0 then 0
          else
            Int.min n (config_callstack_mask lsr config_callstack_shift)
            lsl config_callstack_shift
    in
    let config = Atomic.make (config_one lor callstack lor terminated) in
    let bundle = Computation.Packed (Computation.create ~mode:`LIFO ()) in
    let errors = Control.Errors.create () in
    let finished = Trigger.create () in
    Bundle { config; bundle; errors; finished }
  in
  let fiber = Fiber.current () in
  let outer =
    match pass with
    | Arg -> Nothing
    | FLS -> Fiber.FLS.get fiber flock_key ~default:Nothing
  in
  let (Packed parent as packed) = Fiber.get_computation fiber in
  let (Packed bundle) = r.bundle in
  let canceler = Computation.attach_canceler ~from:parent ~into:bundle in
  (* Ideally there should be no poll point betweem [attach_canceler] and the
     [match ... with] below. *)
  match
    Fiber.set_computation fiber r.bundle;
    fn (match pass with FLS -> Fiber.FLS.set fiber flock_key t | Arg -> t)
  with
  | value ->
      let config = Atomic.get r.config in
      if config land config_terminated_bit <> 0 then begin
        let callstack =
          let n =
            (config land config_callstack_mask) lsr config_callstack_shift
          in
          if n = 0 then None else Some n
        in
        terminate ?callstack t
      end;
      await t fiber packed canceler outer pass;
      value
  | exception exn ->
      let bt = Printexc.get_raw_backtrace () in
      error t exn bt;
      await t fiber packed canceler outer pass;
      Printexc.raise_with_backtrace exn bt

let rec incr (Bundle r as t : t) backoff =
  let before = Atomic.get r.config in
  if before < config_one then completed ()
  else if not (Atomic.compare_and_set r.config before (before + config_one))
  then incr t (Backoff.once backoff)

let finish (Bundle { bundle = Packed bundle; _ } as t : t) canceler =
  Computation.detach bundle canceler;
  decr t

(** This helps to reduce CPU stack usage with the native compiler. *)
let[@inline never] raised exn child t canceler =
  let bt = Printexc.get_raw_backtrace () in
  Computation.cancel child exn bt;
  error t exn bt;
  finish t canceler

(** This helps to reduce CPU stack usage with the native compiler. *)
let[@inline never] returned value child t canceler =
  Computation.return child value;
  finish t canceler

let fork_as_promise_pass (type a) (Bundle r as t : t) thunk (pass : a pass) =
  (* The sequence of operations below ensures that nothing is leaked. *)
  incr t Backoff.default;
  try
    let child = Computation.create ~mode:`LIFO () in
    let fiber = Fiber.create ~forbid:false child in
    let (Packed bundle) = r.bundle in
    let canceler = Computation.attach_canceler ~from:bundle ~into:child in
    let main =
      match pass with
      | FLS -> begin
          Fiber.FLS.set fiber flock_key t;
          fun fiber ->
            match thunk () with
            | value -> returned value child (get_flock fiber) canceler
            | exception exn -> raised exn child (get_flock fiber) canceler
        end
      | Arg -> begin
          fun _ ->
            match thunk () with
            | value -> returned value child t canceler
            | exception exn -> raised exn child t canceler
        end
    in
    Fiber.spawn fiber main;
    child
  with canceled_exn ->
    (* We don't worry about detaching the [canceler], because at this point we
       know the bundle computation has completed or there is something more
       serious. *)
    decr t;
    raise canceled_exn

(** This helps to reduce CPU stack usage with the native compiler. *)
let[@inline never] raised_flock exn fiber =
  let t = get_flock fiber in
  let bt = Printexc.get_raw_backtrace () in
  error t exn bt;
  decr t

(** This helps to reduce CPU stack usage with the native compiler. *)
let[@inline never] raised_bundle exn t =
  error t exn (Printexc.get_raw_backtrace ());
  decr t

let fork_pass (type a) (Bundle r as t : t) thunk (pass : a pass) =
  (* The sequence of operations below ensures that nothing is leaked. *)
  incr t Backoff.default;
  try
    let fiber = Fiber.create_packed ~forbid:false r.bundle in
    let main =
      match pass with
      | FLS -> begin
          Fiber.FLS.set fiber flock_key t;
          fun fiber ->
            match thunk () with
            | () -> decr (get_flock fiber)
            | exception exn -> raised_flock exn fiber
        end
      | Arg -> begin
          fun _ ->
            match thunk () with
            | () -> decr t
            | exception exn -> raised_bundle exn t
        end
    in
    Fiber.spawn fiber main
  with canceled_exn ->
    decr t;
    raise canceled_exn

(* *)

let is_running (Bundle { bundle = Packed bundle; _ } : t) =
  Computation.is_running bundle

let join_after ?callstack ?on_return fn =
  join_after_pass ?callstack ?on_return fn Arg

let fork t thunk = fork_pass t thunk Arg
let fork_as_promise t thunk = fork_as_promise_pass t thunk Arg

let unsafe_incr (Bundle r : t) =
  Atomic.fetch_and_add r.config config_one |> ignore
