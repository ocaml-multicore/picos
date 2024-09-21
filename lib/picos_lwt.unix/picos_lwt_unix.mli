(** Direct style {!Picos} compatible interface to {!Lwt} with {!Lwt_unix} for
    OCaml 5. *)

open Picos

val run_fiber : Fiber.t -> (Fiber.t -> unit) -> unit Lwt.t
(** [run_fiber fiber main] runs the [main] program as the specified [fiber] as a
    promise with {!Lwt} as the scheduler using a {!Lwt_unix} based
    {{!Picos_lwt.System} [System]} module.  In other words, the [main] program
    will be run as a {!Lwt} promise or fiber.

    ⚠️ This may only be called on the main thread on which {!Lwt} runs. *)

val run : ?forbid:bool -> (unit -> 'a) -> 'a Lwt.t
(** [run main] is equivalent to calling {!run_fiber} with a freshly created
    fiber and [main] wrapped to capture the result of [main].

    The optional [forbid] argument defaults to [false] and determines whether
    propagation of cancelation is initially allowed. *)

val run_main : ?forbid:bool -> (unit -> 'a) -> 'a
(** [run_main main] is equivalent to {{!run} [Lwt_main.run (run main)]}. *)
