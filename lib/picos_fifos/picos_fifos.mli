(** Basic single-threaded effects based {!Picos} compatible scheduler for OCaml
    5.

    ℹ️ This scheduler implementation is mostly meant as an example and for use in
    testing libraries implemented in {!Picos}.

    Briefly:

    - {{!Picos.Fiber.current} [current]} returns the current fiber.
    - {{!Picos.Fiber.spawn} [spawn]} forks a new deep effect handler for each
      fiber.
    - {{!Picos.Fiber.yield} [yield]} pushes the current fiber to the back of the
      internal queue and takes the next fiber to run from the head of the
      internal queue.
    - {{!Picos.Computation.cancel_after} [cancel_after]} uses a per-domain
      {{!Picos_select} background thread} that runs a {!Unix.select} loop to
      cancel computations.
    - {{!Picos.Trigger.await} [await]} stores the current fiber to be resumed
      through the trigger and takes next fiber to run from the head of the
      internal queue.

    This scheduler also gives priority to fibers woken up from
    {{!Picos.Trigger.await} [await]} due to being canceled. *)

val run : ?forbid:bool -> (unit -> 'a) -> 'a
(** [run main] runs the [main] thunk with the scheduler.  Returns after [main]
    and all of the fibers spawned by [main] have returned.

    The optional [forbid] argument defaults to [false] and determines whether
    propagation of cancelation is initially allowed. *)
