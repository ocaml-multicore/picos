module type Trigger = sig
  type t
  type exn_bt

  (** Schedulers may handle the {!Await} effect to customize the behavior of
      [await].

      In case the fiber permits propagation of cancelation, the trigger should
      be attached to the computation of the fiber for the duration of suspending
      the fiber.

      Typically the handler calls {!on_signal} to attach a scheduler specific
      [resume] action to the [trigger].

      Whether being resumed due to cancelation or not, the trigger should be
      either {{!signal} signaled} outside of the effect handler, or {{!dispose}
      disposed} by the effect handler, before resuming the fiber.

      The scheduler is free to choose which ready fiber to resume next. *)
  type _ Effect.t += private Await : t -> exn_bt option Effect.t
end

module type Computation = sig
  type _ t
  type exn_bt

  (** Schedulers may handle the {!Cancel_after} effect to customize the behavior
      of [cancel_after].

      The scheduler should typically attach a trigger to the computation passed
      with the effect and arrange the operation to be canceled upon signal.

      The scheduler should measure time using a monotonic clock.

      In case the fiber permits propagation of cancelation and the computation
      associated with the fiber has been canceled the scheduler is free to
      discontinue the fiber before setting up the timeout. *)
  type _ Effect.t +=
    private
    | Cancel_after : {
        seconds : float;  (** Guaranteed to be non-negative. *)
        exn_bt : exn_bt;
        computation : 'a t;
      }
        -> unit Effect.t
end

module type Fiber = sig
  type t
  type _ computation
  type exn_bt

  val resume : t -> (exn_bt option, 'r) Effect.Deep.continuation -> 'r
  (** [resume fiber k] is equivalent to
      [Effect.Deep.continue k (Fiber.canceled t)]. *)

  val resume_with :
    t ->
    (exn_bt option, 'b) Effect.Shallow.continuation ->
    ('b, 'r) Effect.Shallow.handler ->
    'r
  (** [resume_with fiber k h] is equivalent to
      [Effect.Shallow.continue_with k (Fiber.canceled t) h]. *)

  val continue : t -> ('v, 'r) Effect.Deep.continuation -> 'v -> 'r
  (** [continue fiber k v] is equivalent to:
      {[
        match Fiber.canceled fiber with
        | None -> Effect.Deep.continue k v
        | Some exn_bt -> Exn_bt.discontinue k exn_bt
      ]} *)

  val continue_with :
    t ->
    ('v, 'b) Effect.Shallow.continuation ->
    'v ->
    ('b, 'r) Effect.Shallow.handler ->
    'r
  (** [continue_with fiber k v h] is equivalent to:
      {[
        match Fiber.canceled fiber with
        | None -> Effect.Shallow.continue_with k v h
        | Some exn_bt -> Exn_bt.discontinue_with k exn_bt h
      ]} *)

  (** Schedulers may handle the {!Current} effect to customize the behavior of
      [current].

      ⚠️ A handler should eventually resume the fiber without propagating
      cancelation.  A scheduler may, of course, decide to reschedule the current
      fiber to be resumed later.

      Note that in typical use cases of {!current} it makes sense to give
      priority to the fiber performing {!Current}, but this is not required. *)
  type _ Effect.t += private Current : t Effect.t

  (** Schedulers may handle the {!Yield} effect to customize the behavior of
      [yield].

      Just like with {!Current}, a handler should either continue or discontinue
      the fiber, but, unlike with {!Current}, the scheduler should give priority
      to running other ready fibers before resuming the fiber performing
      {!Yield}. *)
  type _ Effect.t += private Yield : unit Effect.t

  (** Schedulers may handle the {!Spawn} effect to customize the behavior of
      [spawn].

      The scheduler is free to run the newly created fibers on any domain and
      decide which fiber to give priority to. *)
  type _ Effect.t +=
    private
    | Spawn : {
        forbid : bool;
        computation : 'a computation;
        mains : (unit -> unit) list;
      }
        -> unit Effect.t
end
