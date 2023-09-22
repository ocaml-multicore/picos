include Bootstrap.Trigger

type as_signal = [ `Signal ] t

let is_initial t = Atomic.get t == Initial

type _ Effect.t += Await : [ `On | `Signal ] t -> Exn_bt.t option Effect.t

open struct
  let[@inline never] awaiting () = invalid_arg "Trigger: already awaiting"
end

let rec on_signal t x y action =
  match Atomic.get t with
  | Initial ->
      Atomic.compare_and_set t Initial (Awaiting (action, x, y))
      || on_signal t x y action
  | Signaled -> false
  | Awaiting _ -> awaiting ()

open struct
  let release _ _ (per_thread : Per_thread.t) =
    Mutex.lock per_thread.mutex;
    Mutex.unlock per_thread.mutex;
    Condition.broadcast per_thread.condition

  let block trigger (per_thread : Per_thread.t) =
    Mutex.lock per_thread.mutex;
    match
      while Atomic.get trigger != Signaled do
        Condition.wait per_thread.condition per_thread.mutex
      done
    with
    | () ->
        Mutex.unlock per_thread.mutex;
        Bootstrap.Fiber.canceled per_thread.fiber
    | exception exn ->
        (* Condition.wait may be interrupted by asynchronous exceptions and we
           must make sure to unlock even in that case. *)
        Mutex.unlock per_thread.mutex;
        raise exn

  let await_default trigger =
    let per_thread = Per_thread.get () in
    if Bootstrap.Fiber.has_forbidden per_thread.fiber then
      if on_signal trigger () per_thread release then block trigger per_thread
      else None
    else if Bootstrap.Fiber.try_attach per_thread.fiber trigger then
      if on_signal trigger () per_thread release then block trigger per_thread
      else begin
        Bootstrap.Fiber.detach per_thread.fiber trigger;
        Bootstrap.Fiber.canceled per_thread.fiber
      end
    else begin
      signal trigger;
      Bootstrap.Fiber.canceled per_thread.fiber
    end
end

let await t =
  match Atomic.get t with
  | Initial -> begin
      try Effect.perform (Await t)
      with Effect.Unhandled (Await t) -> await_default t
    end
  | Signaled -> None
  | Awaiting _ -> awaiting ()
