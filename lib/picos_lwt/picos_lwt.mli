(** A {!Picos} compatible direct style interface to {!Lwt} with given
    implementation of [sleep].

    This basically gives you an alternative direct style interface to
    programming with {!Lwt}.  All the scheduling decisions will be made by
    {!Lwt}. *)

val await : (unit -> 'a Lwt.t) -> 'a
(** [await thunk] awaits for the promise returned by [thunk ()] to resolve and
    returns the result.  This should only be called from inside a fiber started
    through {!run}. *)

val run :
  ?forbid:bool -> sleep:(float -> unit Lwt.t) -> (unit -> 'a) -> 'a Lwt.t
(** [run ~sleep main] runs the [main] program implemented in {!Picos} as a
    promise with {!Lwt} as the scheduler and given operation to [sleep].  In
    other words, the [main] program will be run as a {!Lwt} promise or fiber.

    Calling [sleep seconds] should return a cancelable promise that resolves
    after given number of [seconds] (unless canceled).

    ℹ️ Inside [main] you can use anything implemented in Picos for concurrent
    programming.  In particular, you only need to call [run] with an
    implementation of [sleep] at the entry point of your application.

    The optional [forbid] argument defaults to [false] and determines whether
    propagation of cancelation is initially allowed. *)

(** {1 Functorized interface}

    A functor for building a {!Picos} compatible direct style interface to
    {!Lwt} with given implementation of {{!Sleep} sleep}. *)

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
[@@deprecated
  "Just use Picos_lwt.run instead. This functorized interface will be removed."]
