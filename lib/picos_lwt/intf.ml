module type Sleep = sig
  (** Minimal signature for an implementation of {!sleep} using {!Lwt}. *)

  val sleep : float -> unit Lwt.t
  (** [sleep seconds] should return a cancelable promise that resolves after
      given number of [seconds] (unless canceled). *)
end

module type S = sig
  (** Direct style {!Picos} compatible interface to {!Lwt}. *)

  val run : forbid:bool -> (unit -> 'a) -> 'a Lwt.t
  (** [run ~forbid main] runs the [main] program implemented in {!Picos} as a
      promise with {!Lwt} as the scheduler.  In other words, the [main] program
      will be run as a {!Lwt} promise or fiber. *)

  val await : (unit -> 'a Lwt.t) -> 'a
  (** [await thunk] awaits for the promise returned by [thunk ()] to resolve and
      returns the result.  This should only be called from inside a fiber
      started through {!run}. *)
end
