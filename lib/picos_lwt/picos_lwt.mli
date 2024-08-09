(** Direct style {!Picos} compatible interface to {!Lwt} for OCaml 5.

    This basically gives you an alternative direct style interface to
    programming with {!Lwt}.  All the scheduling decisions will be made by
    {!Lwt}.

    ℹ️ This is a {{!System} system} independent interface to {!Lwt}.  See
    {!Picos_lwt_unix} for a {!Unix} specific interface. *)

open Picos

val await : 'a Lwt.t -> 'a
(** [await promise] awaits for the [promise] to resolve and returns the result.

    ⚠️ This may only be called on the main thread on which {!Lwt} runs from
    inside a fiber started through {!run}. *)

include module type of Intf

val run_fiber : (module System) -> Fiber.t -> (Fiber.t -> unit) -> unit Lwt.t
(** [run_fiber (module System) fiber main] runs the [main] program as the
    specified [fiber] as a promise with {!Lwt} as the scheduler using the given
    {!System} module.  In other words, the [main] program will be run as a
    {!Lwt} promise or fiber.

    ℹ️ Inside [main] you can use anything implemented in Picos for concurrent
    programming.  In particular, you only need to call [run] with a {!System}
    module implementation at the entry point of your application.

    ⚠️ This may only be called on the main thread on which {!Lwt} runs. *)

val run : ?forbid:bool -> (module System) -> (unit -> 'a) -> 'a Lwt.t
(** [run (module System) main] is equivalent to calling {!run_fiber} with a
    freshly created fiber and [main] wrapped to capture the result of [main].

    The optional [forbid] argument defaults to [false] and determines whether
    propagation of cancelation is initially allowed. *)
