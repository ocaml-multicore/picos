(** A functor for building a {!Picos} compatible direct style interface to
    {!Lwt} with given implementation of {{!Sleep} sleep}.

    This basically gives you an alternative direct style interface to
    programming with {!Lwt}.  All the scheduling decisions will be made by
    {!Lwt}. *)

include module type of Intf

(** [Make (Sleep)] creates a {!Picos} compatible interface to {!Lwt} with given
    implementation of {{!Sleep} sleep}.

    For example,
    {[
      module Picos_lwt_unix = Picos_lwt.Make (Lwt_unix)
    ]}
    instantiates this functor using {!Lwt_unix.sleep} as the implemention of
    {{!Sleep.sleep} sleep}. *)
module Make : functor (_ : Sleep) -> S
