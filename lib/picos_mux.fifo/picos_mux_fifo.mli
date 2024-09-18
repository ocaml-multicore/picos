(** Basic single-threaded effects based {!Picos} compatible scheduler for OCaml
    5.

    This scheduler uses a queue specifically optimized for a single-threaded
    scheduler to implement a basic FIFO scheduler.  This scheduler also gives
    priority to fibers woken up due to being canceled.

    🐌 Due to FIFO scheduling this scheduler performs poorly on highly parallel
    workloads.

    ℹ️ See {!Picos_mux_multififo} for a multi-threaded variation of this
    scheduler.

    ℹ️ This scheduler implementation is mostly meant as an example and for use in
    testing libraries implemented in {!Picos}.

    ⚠️ This scheduler uses {!Picos_io_select} internally.  If running multiple
    threads that each run this scheduler, {!Picos_io_select.configure} must be
    called by the main thread before creating other threads. *)

open Picos

val run_fiber :
  ?quota:int ->
  ?fatal_exn_handler:(exn -> unit) ->
  Fiber.t ->
  (Fiber.t -> unit) ->
  unit
(** [run_fiber fiber main] runs the [main] program as the specified [fiber] and
    returns after [main] and all of the fibers spawned by [main] have returned.

    The optional [quota] argument defaults to [Int.max_int] and determines the
    number of effects a fiber is allowed to perform before it is forced to
    yield. *)

val run :
  ?quota:int ->
  ?fatal_exn_handler:(exn -> unit) ->
  ?forbid:bool ->
  (unit -> 'a) ->
  'a
(** [run main] is equivalent to calling {!run_fiber} with a freshly created
    fiber and [main] wrapped to capture the result of [main].

    The optional [forbid] argument defaults to [false] and determines whether
    propagation of cancelation is initially allowed. *)
