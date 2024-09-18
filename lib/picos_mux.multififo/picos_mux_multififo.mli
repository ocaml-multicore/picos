(** Basic multi-threaded effects based {!Picos} compatible scheduler for OCaml
    5.

    This scheduler uses a queue per thread to implement a mostly FIFO scheduler.
    If a thread runs out of fibers to run, it will try to take a fiber from the
    queues of other threads, which means that fibers can move from one thread to
    another.  This scheduler also gives priority to fibers woken up due to being
    canceled.

    🐌 Due to mostly FIFO scheduling this scheduler performs poorly on highly
    parallel workloads.

    ℹ️ See {!Picos_mux_fifo} for a single-threaded variation of this scheduler.

    ℹ️ This scheduler implementation is mostly meant as an example and for use in
    testing libraries implemented in {!Picos}.

    ⚠️ This scheduler uses {!Picos_io_select} internally.  If running multiple
    threads that each run this scheduler, {!Picos_io_select.configure} must be
    called by the main thread before creating other threads. *)

open Picos

type t
(** Represents a shared context for fifo runners. *)

val context : ?quota:int -> ?fatal_exn_handler:(exn -> unit) -> unit -> t
(** [context ()] creates a new context for randomized runners.  The context
    should be consumed by a call of {{!run} [run ~context ...]}.

    The optional [quota] argument defaults to [Int.max_int] and determines the
    number of effects a fiber is allowed to perform before it is forced to
    yield. *)

val runner_on_this_thread : t -> unit
(** [runner_on_this_thread context] starts a runner on the current thread to run
    fibers on the context.  The runner returns when {{!run} [run ~context ...]}
    returns. *)

val run_fiber : ?context:t -> Fiber.t -> (Fiber.t -> unit) -> unit
(** [run_fiber fiber main] runs the [main] program as the specified [fiber] and
    returns after [main] and all of the fibers spawned by [main] have
    returned. *)

val run : ?context:t -> ?forbid:bool -> (unit -> 'a) -> 'a
(** [run main] is equivalent to calling {!run_fiber} with a freshly created
    fiber and [main] wrapped to capture the result of [main].

    The optional [forbid] argument defaults to [false] and determines whether
    propagation of cancelation is initially allowed. *)

val run_fiber_on :
  ?quota:int ->
  ?fatal_exn_handler:(exn -> unit) ->
  n_domains:int ->
  Fiber.t ->
  (Fiber.t -> unit) ->
  unit
(** [run_fiber_on ~n_domains main] spawns [n_domains - 1] additional domains and
    runs the [main] on the current domain and those additional domains. *)

val run_on :
  ?quota:int ->
  ?fatal_exn_handler:(exn -> unit) ->
  n_domains:int ->
  ?forbid:bool ->
  (unit -> 'a) ->
  'a
(** [run_on ~n_domains main] is equivalent to calling {!run_fiber_on} with a
    freshly created fiber and [main] wrapped to capture the result of [main].

    The optional [forbid] argument defaults to [false] and determines whether
    propagation of cancelation is initially allowed. *)
