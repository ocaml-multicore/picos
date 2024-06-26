(** Direct style {!Picos} compatible interface to {!Lwt} with {!Lwt_unix} for
    OCaml 5. *)

val run : ?forbid:bool -> (unit -> 'a) -> 'a Lwt.t
(** [run main] runs the [main] program implemented in {!Picos} as a promise with
    {!Lwt} as the scheduler using a {!Lwt_unix} based {{!Picos_lwt.System}
    [System]} module.

    ⚠️ This may only be called on the main thread on which {!Lwt} runs. *)
