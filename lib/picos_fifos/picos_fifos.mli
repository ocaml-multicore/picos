(** A basic single-threaded effects based {!Picos} compatible scheduler for
    OCaml 5.

    ℹ️ This scheduler implementation is mostly meant as an example and for use in
    testing libraries implemented in {!Picos}.

    Briefly:

    - [current] returns the current fiber.
    - [spawn] forks a new deep effect handler for each fiber.
    - [yield] pushes the current fiber to the back of the internal queue and
      takes the next fiber to run from the head of the internal queue.
    - [cancel_after] uses a per-domain {{!Picos_select} background thread} that
      runs a [Unix.select] loop to cancel computations.
    - [await] stores the current fiber to be resumed through the trigger and
      takes next fiber to run from the head of the internal queue.

    This scheduler also gives priority to fibers woken up from [await] due to
    being canceled. *)

val run : forbid:bool -> (unit -> 'a) -> 'a
(** [run ~forbid main] runs the [main] thunk with the scheduler.  Returns after
    [main] and all of the fibers spawned by [main] have returned. *)
