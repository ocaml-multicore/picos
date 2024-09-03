(** Basic {!Thread} based {!Picos} compatible scheduler for OCaml 4.

    ℹ️ This scheduler implementation is mostly meant as an example and for use in
    testing libraries implemented in {!Picos}.

    ⚠️ This scheduler uses {!Picos_io_select} internally.  If running multiple
    threads that each run this scheduler, {!Picos_io_select.configure} must be
    called by the main thread before creating other threads.

    ⚠️ This scheduler is probably suitable for simple applications that do not
    spawn a lot of fibers.  If an application uses a lot of short lived fibers,
    then a more sophisticated scheduler implementation using some sort of thread
    pool will likely perform significantly better.

    ⚠️ This scheduler implementation also works on OCaml 5, of course, but on
    OCaml 5 a scheduler that implements an effect handler directly is likely to
    perform better. *)

open Picos

val run_fiber :
  ?fatal_exn_handler:(exn -> unit) -> Fiber.t -> (Fiber.t -> unit) -> unit
(** [run_fiber fiber main] runs the [main] program as the specified [fiber] and
    returns [main] and all of the fibers spawned by [main] have returned. *)

val run : ?forbid:bool -> ?fatal_exn_handler:(exn -> unit) -> (unit -> 'a) -> 'a
(** [run main] is equivalent to calling {!run_fiber} with a freshly created
    fiber and [main] wrapped to capture the result of [main].

    The optional [forbid] argument defaults to [false] and determines whether
    propagation of cancelation is initially allowed. *)
