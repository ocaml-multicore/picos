module type Trigger = sig
  type t
  type exn_bt

  (** Schedulers must handle the {!Await} effect to implement the behavior of
      {!await}.

      In case the fiber permits propagation of cancelation, the trigger must be
      attached to the computation of the fiber for the duration of suspending
      the fiber by the scheduler.

      Typically the scheduler calls {{!Fiber.try_suspend} [try_suspend]}, which
      in turn calls {!on_signal}, to attach a scheduler specific [resume] action
      to the [trigger].  The scheduler must guarantee that the fiber will be
      resumed after {!signal} has been called on the [trigger].

      Whether being resumed due to cancelation or not, the trigger must be
      either {{!signal} signaled} outside of the effect handler, or {{!dispose}
      disposed} by the effect handler, before resuming the fiber.

      In case the fiber permits propagation of cancelation and the computation
      associated with the fiber has been canceled the scheduler is free to
      continue the fiber immediately with the cancelation exception after
      {{!dispose} disposing} the trigger.

      ⚠️ A scheduler must not discontinue, i.e. raise an exception to, the fiber
      as a response to {!Await}.

      The scheduler is free to choose which ready fiber to resume next. *)
  type _ Effect.t += private Await : t -> exn_bt option Effect.t
end

module type Computation = sig
  type _ t
  type exn_bt

  (** Schedulers must handle the {!Cancel_after} effect to implement the
      behavior of {!cancel_after}.

      The scheduler should typically {{!try_attach} attach} a {{!Trigger}
      trigger} to the computation passed with the effect and arrange the
      timeout to be canceled upon signal to avoid space leaks.

      The scheduler should measure time using a monotonic clock.

      In case the fiber permits propagation of cancelation and the computation
      associated with the fiber has been canceled the scheduler is free to
      discontinue the fiber before setting up the timeout.

      If the fiber is continued normally, i.e. without raising an exception, the
      scheduler should guarantee that the cancelation will be delivered
      eventually.

      The scheduler is free to choose which ready fiber to resume next. *)
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
      {{!Fiber.canceled} [Effect.Deep.continue k (Fiber.canceled t)]}. *)

  val resume_with :
    t ->
    (exn_bt option, 'b) Effect.Shallow.continuation ->
    ('b, 'r) Effect.Shallow.handler ->
    'r
  (** [resume_with fiber k h] is equivalent to
      {{!Fiber.canceled} [Effect.Shallow.continue_with k (Fiber.canceled t) h]}. *)

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

  (** Schedulers must handle the {!Current} effect to implement the behavior of
      {!current}.

      ⚠️ The scheduler must eventually resume the fiber without propagating
      cancelation.  This is necessary to allow a fiber to control the
      propagation of cancelation through the {{!t} fiber}.

      The scheduler is free to choose which ready fiber to resume next.
      However, in typical use cases of {!current} it makes sense to give
      priority to the fiber performing {!Current}, but this is not required. *)
  type _ Effect.t += private Current : t Effect.t

  (** Schedulers must handle the {!Yield} effect to implement the behavior of
      {!yield}.

      In case the fiber permits propagation of cancelation and the computation
      associated with the fiber has been canceled the scheduler is free to
      discontinue the fiber immediately.

      The scheduler should give priority to running other ready fibers before
      resuming the fiber performing {!Yield}.  A scheduler that always
      immediately resumes the fiber performing {!Yield} may prevent an otherwise
      valid program from making progress. *)
  type _ Effect.t += private Yield : unit Effect.t

  (** Schedulers must handle the {!Spawn} effect to implement the behavior of
      {!spawn}.

      In case the fiber permits propagation of cancelation and the computation
      associated with the fiber has been canceled the scheduler is free to
      discontinue the fiber immediately before spawning new fibers.

      The scheduler is free to run the newly created fibers on any domain and
      decide which fiber to give priority to.

      ⚠️ The scheduler should guarantee that, when [Spawn] returns normally, all
      of the [mains] will eventually be called by the scheduler and, when
      [Spawn] raises an exception, none of the [mains] will be called.  In other
      words, [Spawn] should check cancelation just once and be all or nothing.
      Furthermore, in case a newly spawned fiber is canceled before its main is
      called, the scheduler must still call the main.  This allows a program to
      ensure, i.e. keep track of, that all fibers it spawns are terminated
      properly and any resources transmitted to spawned fibers will be disposed
      properly. *)
  type _ Effect.t +=
    private
    | Spawn : {
        forbid : bool;
        computation : 'a computation;
        mains : (unit -> unit) list;
      }
        -> unit Effect.t
end
