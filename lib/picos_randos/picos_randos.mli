(** Randomized single-threaded effects based {!Picos} compatible scheduler for
    OCaml 5.

    ℹ️ This scheduler implementation is specifically intended for testing
    libraries implemented in Picos.

    {!Picos} is an interface that allows schedulers to make scheduling decisions
    freely.  After each effect this scheduler picks the next fiber to run
    randomly from the collection of ready fibers.  This can help to discover
    bugs in programs implemented in Picos that make invalid scheduling
    assumptions. *)

val run : ?forbid:bool -> (unit -> 'a) -> 'a
(** [run main] runs the [main] thunk with the scheduler.  Returns after [main]
    and all of the fibers spawned by [main] have returned.

    The optional [forbid] argument defaults to [false] and determines whether
    propagation of cancelation is initially allowed. *)
