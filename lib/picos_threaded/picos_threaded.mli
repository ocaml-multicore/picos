(** A basic {!Thread} based {!Picos} compatible scheduler for OCaml 4.

    ℹ️ This scheduler implementation is mostly meant as an example and for use in
    testing libraries implemented in {!Picos}.

    Briefly:

    - [current] returns the current fiber from the per thread state.
    - [spawn] creates a new [Thread] for each fiber.
    - [yield] just calls {!Thread.yield}.
    - [cancel_after] uses a per-domain {{!Picos_select} background thread} that
      runs a [Unix.select] loop to cancel computations.
    - [await] uses {{!Picos_ptmc} a per thread mutex and condition} to suspend
      the thread.

    ⚠️ This scheduler is probably suitable for simple applications that do not
    spawn a lot of fibers.  If an application uses a lot of short lived fibers,
    then a more sophisticated scheduler implementation using some sort of thread
    pool will likely perform significantly better.

    ⚠️ This scheduler implementation also works on OCaml 5, of course, but on
    OCaml 5 a scheduler that implements an effect handler directly is likely to
    perform better. *)

val run : forbid:bool -> (unit -> 'a) -> 'a
(** [run ~forbid main] runs the [main] thunk with the scheduler.  Returns after
    [main] and all of the fibers spawned by [main] have returned. *)
