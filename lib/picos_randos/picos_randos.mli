(** Randomized multi-threaded effects based {!Picos} compatible scheduler for
    OCaml 5.

    ℹ️ This scheduler implementation is specifically intended for testing
    libraries implemented in Picos.

    {!Picos} is an interface that allows schedulers to make scheduling decisions
    freely.  After each effect this scheduler picks the next fiber to run
    randomly from the collection of ready fibers.  This can help to discover
    bugs in programs implemented in Picos that make invalid scheduling
    assumptions. *)

open Picos

type t
(** Represents a shared context for randomized runners. *)

val context : ?fatal_exn_handler:(exn -> unit) -> unit -> t
(** [context ()] creates a new context for randomized runners.  The context
    should be consumed by a call of {{!run} [run ~context ...]}. *)

val runner_on_this_thread : t -> unit
(** [runner_on_this_thread context] starts a runner on the current thread to run
    fibers on the context.  The runner returns when {{!run} [run ~context ...]}
    returns. *)

val run_fiber : ?context:t -> Fiber.t -> (Fiber.t -> unit) -> unit
(** [run_fiber fiber main] runs the [main] program as the specified [fiber] and
    returns [main] and all of the fibers spawned by [main] have returned.

    The optional [context] argument specifies a context in which to run the
    [main] program.  If unspecified, a new context is automatically created and
    the scheduler will be single-threaded.  By {{!context} creating a context},
    spawning concurrent or parallel {{!runner_on_this_thread} runners} on to the
    context, and then explicitly passing the context to [run ~context ...] one
    can create a multi-threaded scheduler.  Only a single call of {!run} per
    context is allowed. *)

val run : ?context:t -> ?forbid:bool -> (unit -> 'a) -> 'a
(** [run main] is equivalent to calling {!run_fiber} with a freshly created
    fiber and [main] wrapped to capture the result of [main].

    The optional [forbid] argument defaults to [false] and determines whether
    propagation of cancelation is initially allowed. *)

val run_fiber_on :
  ?fatal_exn_handler:(exn -> unit) ->
  n_domains:int ->
  Fiber.t ->
  (Fiber.t -> unit) ->
  unit
(** [run_fiber_on ~n_domains main] spawns [n_domains - 1] additional domains and
    runs the [main] on the current domain and those additional domains. *)

val run_on :
  ?fatal_exn_handler:(exn -> unit) ->
  n_domains:int ->
  ?forbid:bool ->
  (unit -> 'a) ->
  'a
(** [run_on ~n_domains main] is equivalent to calling {!run_fiber_on} with a
    freshly created fiber and [main] wrapped to capture the result of [main].

    The optional [forbid] argument defaults to [false] and determines whether
    propagation of cancelation is initially allowed. *)
