open Picos

let[@inline never] completed () = invalid_arg "already completed"

type _ tdt =
  | Nothing : [> `Nothing ] tdt
  | Bundle : {
      mutable _config : int;
      bundle : Computation.packed;
      errors : Control.Errors.t;
      mutable finished : Trigger.t;
    }
      -> [> `Bundle ] tdt

type t = [ `Bundle ] tdt

external config_as_atomic : t -> int Atomic.t = "%identity"

let config_on_return_terminate_bit = 0x01
and config_on_terminate_raise_bit = 0x02
and config_callstack_mask = 0x6C
and config_callstack_shift = 2
and config_one = 0x80 (* memory runs out before overflow *)

let flock_key : [ `Bundle | `Nothing ] tdt Fiber.FLS.t = Fiber.FLS.create ()

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
  else if
    Atomic.get (config_as_atomic t) land config_on_terminate_raise_bit <> 0
  then terminate ?callstack t

let decr (Bundle r as t : t) =
  let n = Atomic.fetch_and_add (config_as_atomic t) (-config_one) in
  if n < config_one * 2 then begin
    Trigger.signal r.finished
  end

type _ pass = FLS : unit pass | Arg : t pass

let[@inline never] no_flock () = invalid_arg "no flock"

let[@inline] on_terminate = function
  | None | Some `Ignore -> `Ignore
  | Some `Raise -> `Raise

let get_flock fiber =
  match Fiber.FLS.get fiber flock_key ~default:Nothing with
  | Bundle _ as t -> t
  | Nothing -> no_flock ()

let await (Bundle r as t : t) fiber packed canceler outer =
  Fiber.set_computation fiber packed;
  if Fiber.FLS.get fiber flock_key ~default:Nothing != outer then
    Fiber.FLS.set fiber flock_key outer;
  let forbid = Fiber.exchange fiber ~forbid:true in
  let n = Atomic.fetch_and_add (config_as_atomic t) (-config_one) in
  if config_one * 2 <= n then begin
    r.finished <- Trigger.create ();
    (* The [fetch_and_add] below provides a full fence that prevents the above
       write from being delayed after the [Trigger.await] below. *)
    if config_one <= Atomic.fetch_and_add (config_as_atomic t) 0 then
      Trigger.await r.finished |> ignore
  end;
  Fiber.set fiber ~forbid;
  let (Packed parent) = packed in
  Computation.detach parent canceler;
  Control.Errors.check r.errors;
  begin
    let (Packed bundle) = r.bundle in
    match Computation.peek_exn bundle with
    | _ -> ()
    | exception Computation.Running ->
        Computation.cancel bundle Control.Terminate Control.empty_bt
    | exception Control.Terminate
      when Atomic.get (config_as_atomic t) land config_on_terminate_raise_bit
           = 0 ->
        ()
  end;
  Fiber.check fiber

let[@inline never] raised exn t fiber packed canceler outer =
  let bt = Printexc.get_raw_backtrace () in
  error t exn bt;
  await t fiber packed canceler outer;
  Printexc.raise_with_backtrace exn bt

let[@inline never] returned value (t : t) fiber packed canceler outer =
  let config = Atomic.get (config_as_atomic t) in
  if config land config_on_return_terminate_bit <> 0 then begin
    let callstack =
      let n = (config land config_callstack_mask) lsr config_callstack_shift in
      if n = 0 then None else Some n
    in
    terminate ?callstack t
  end;
  await t fiber packed canceler outer;
  value

let join_after_realloc x fn t fiber packed canceler outer =
  match fn x with
  | value -> returned value t fiber packed canceler outer
  | exception exn -> raised exn t fiber packed canceler outer

let join_after_pass (type a) ?callstack ?on_return ?on_terminate (fn : a -> _)
    (pass : a pass) =
  (* The sequence of operations below ensures that nothing is leaked. *)
  let (Bundle r as t : t) =
    let config =
      match on_return with
      | None | Some `Wait -> config_one
      | Some `Terminate -> config_one lor config_on_return_terminate_bit
    in
    let config =
      match on_terminate with
      | None | Some `Ignore -> config
      | Some `Raise -> config lor config_on_terminate_raise_bit
    in
    let config =
      match callstack with
      | None -> config
      | Some n ->
          if n <= 0 then config
          else
            config
            lor Int.min n (config_callstack_mask lsr config_callstack_shift)
                lsl config_callstack_shift
    in
    let bundle = Computation.Packed (Computation.create ~mode:`LIFO ()) in
    let errors = Control.Errors.create () in
    let finished = Trigger.signaled in
    Bundle { _config = config; bundle; errors; finished }
  in
  let fiber = Fiber.current () in
  let outer = Fiber.FLS.get fiber flock_key ~default:Nothing in
  begin
    match pass with FLS -> Fiber.FLS.reserve fiber flock_key | Arg -> ()
  end;
  let (Packed parent as packed) = Fiber.get_computation fiber in
  let (Packed bundle) = r.bundle in
  let canceler = Computation.attach_canceler ~from:parent ~into:bundle in
  (* Ideally there should be no poll point betweem [attach_canceler] and the
     [match ... with] in [join_after_realloc]. *)
  Fiber.set_computation fiber r.bundle;
  let x : a =
    match pass with FLS -> Fiber.FLS.set fiber flock_key t | Arg -> t
  in
  join_after_realloc x fn t fiber packed canceler outer

let rec incr (t : t) backoff =
  let before = Atomic.get (config_as_atomic t) in
  if before < config_one then completed ()
  else if
    not
      (Atomic.compare_and_set (config_as_atomic t) before (before + config_one))
  then incr t (Backoff.once backoff)

let finish (Bundle { bundle = Packed bundle; _ } as t : t) canceler =
  Computation.detach bundle canceler;
  decr t

let[@inline never] raised exn child t canceler =
  let bt = Printexc.get_raw_backtrace () in
  Computation.cancel child exn bt;
  error t exn bt;
  finish t canceler

let[@inline never] returned value child t canceler =
  Computation.return child value;
  finish t canceler

let[@inline never] plug t thunk child canceler =
  match thunk () with
  | value -> returned value child t canceler
  | exception exn -> raised exn child t canceler

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
      | FLS ->
          Fiber.FLS.set fiber flock_key t;
          fun fiber -> plug (get_flock fiber) thunk child canceler
      | Arg -> fun _ -> plug t thunk child canceler
    in
    Fiber.spawn fiber main;
    child
  with canceled_exn ->
    (* We don't worry about detaching the [canceler], because at this point we
       know the bundle computation has completed or there is something more
       serious. *)
    decr t;
    raise canceled_exn

let[@inline never] raised exn t =
  error t exn (Printexc.get_raw_backtrace ());
  decr t

let[@inline never] plug t thunk =
  match thunk () with () -> decr t | exception exn -> raised exn t

let fork_pass (type a) (Bundle r as t : t) thunk (pass : a pass) =
  (* The sequence of operations below ensures that nothing is leaked. *)
  incr t Backoff.default;
  try
    let fiber = Fiber.create_packed ~forbid:false r.bundle in
    let main =
      match pass with
      | FLS ->
          Fiber.FLS.set fiber flock_key t;
          fun fiber -> plug (get_flock fiber) thunk
      | Arg -> fun _ -> plug t thunk
    in
    Fiber.spawn fiber main
  with canceled_exn ->
    decr t;
    raise canceled_exn

(* *)

let is_running (Bundle { bundle = Packed bundle; _ } : t) =
  Computation.is_running bundle

let join_after ?callstack ?on_return ?on_terminate fn =
  join_after_pass ?callstack ?on_return ?on_terminate fn Arg

let fork t thunk = fork_pass t thunk Arg
let fork_as_promise t thunk = fork_as_promise_pass t thunk Arg

let unsafe_incr (t : t) =
  Atomic.fetch_and_add (config_as_atomic t) config_one |> ignore
