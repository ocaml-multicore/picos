(** A basic [Thread] based implementation of the effects based operations of
    {!Picos} for OCaml 4.

    Briefly:

    - [current] returns the current fiber from the per thread state.
    - [spawn] creates a new [Thread] for each fiber.
    - [yield] just calls [Thread.yield].
    - [cancel_after] uses {{!Picos_domain.DLS} [DLS]} to store a priority queue
      of timeouts and a per-domain {{!Picos_select} background timeout thread}
      that runs a [Unix.select] loop to cancel computations.
    - [await] uses {{!Picos_ptmc} a per thread mutex and condition} to suspend
      the thread.

    ⚠️ This scheduler implementation is mostly meant as an example and is
    probably suitable for simple applications that do not spawn a lot of fibers.
    If an application uses a lot of short lived fibers, then a more
    sophisticated scheduler implementation using some sort of thread pool will
    likely perform significantly better.

    ⚠️ This scheduler implementation also works on OCaml 5, of course, but on
    OCaml 5 a scheduler that implements an effect handler directly is likely to
    perform better. *)

open Picos

type t
(** The per thread state for a {!handler}. *)

val create : forbid:bool -> 'a Computation.t -> t
(** [create ~forbid computation] creates a new per thread state for a fiber with
    the given [computation] and propagation of cancelation [forbid]den as
    given. *)

val handler : t Handler.t
(** A basic handler of the effects based operations of {!Picos} using [Thread]s
    and a [Unix.select] based IO event loop. *)
