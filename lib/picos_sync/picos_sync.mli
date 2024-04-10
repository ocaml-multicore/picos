(** Basic communication and synchronization primitives for {!Picos}. *)

module Mutex : sig
  (** A mutex implementation for {!Picos}.

      ℹ️ This intentionally mimics the interface of {!Stdlib.Mutex}.  Unlike with
      the standard library mutex, blocking on this mutex potentially allows an
      effects based scheduler to run other fibers on the thread.

      🏎️ The optional [checked] argument taken by most of the operations defaults
      to [true].  When explicitly specified as [~checked:false] the mutex
      implementation may avoid having to obtain the {{!Picos.Fiber.current}
      current fiber}, which can be expensive relative to locking or unlocking an
      uncontested mutex.  Note that specifying [~checked:false] on an operation
      may prevent error checking also on a subsequent operation. *)

  type t
  (** Represents a mutual-exclusion lock or mutex. *)

  val create : ?padded:bool -> unit -> t
  (** [create ()] returns a new mutex that is initially unlocked. *)

  val lock : ?checked:bool -> t -> unit
  (** [lock mutex] locks the [mutex].

      ℹ️ If the fiber has been canceled and propagation of cancelation is
      allowed, this may raise the cancelation exception before locking the
      mutex.  If [~checked:false] was specified, the cancelation exception may
      or may not be raised.

      @raise Sys_error if the mutex is already locked by the fiber.  If
        [~checked:false] was specified for some previous operation on the mutex
        the exception may or may not be raised. *)

  val try_lock : ?checked:bool -> t -> bool
  (** [try_lock mutex] locks the mutex in case the mutex is unlocked.  Returns
      [true] on success and [false] in case the mutex was locked.

      ℹ️ If the fiber has been canceled and propagation of cancelation is
      allowed, this may raise the cancelation exception before locking the
      mutex.  If [~checked:false] was specified, the cancelation exception may
      or may not be raised. *)

  val unlock : ?checked:bool -> t -> unit
  (** [unlock mutex] unlocks the mutex.

      @raise Sys_error if the mutex was locked by another fiber.  If
        [~checked:false] was specified for some previous operation on the mutex
        the exception may or may not be raised. *)

  val protect : ?checked:bool -> t -> (unit -> 'a) -> 'a
  (** [protect mutex thunk] locks the [mutex], runs [thunk ()], and unlocks the
      [mutex] after [thunk ()] returns or raises.

      ℹ️ If the fiber has been canceled and propagation of cancelation is
      allowed, this may raise the cancelation exception before locking the
      mutex.  If [~checked:false] was specified, the cancelation exception may
      or may not be raised.

      @raise Sys_error for the same reasons as {!lock} and {!unlock}. *)
end

module Condition : sig
  (** A condition implementation for {!Picos}.

      ℹ️ This intentionally mimics the interface of {!Stdlib.Condition}.  Unlike
      with the standard library condition variable, blocking on this condition
      variable allows an effects based scheduler to run other fibers on the
      thread. *)

  type t
  (** Represents a condition variable. *)

  val create : ?padded:bool -> unit -> t
  (** [create ()] return a new condition variable. *)

  val wait : t -> Mutex.t -> unit
  (** [wait condition] unlocks the [mutex], waits for the [condition], and locks
      the [mutex] before returning or raising due to the operation being
      canceled.

      ℹ️ If the fiber has been canceled and propagation of cancelation is
      allowed, this may raise the cancelation exception. *)

  val signal : t -> unit
  (** [signal condition] wakes up one fiber waiting on the [condition] variable
      unless there are no such fibers. *)

  val broadcast : t -> unit
  (** [broadcast condition] wakes up all the fibers waiting on the [condition]
      variable. *)
end

module Lazy : sig
  (** A lazy implementation for {!Picos}.

      ℹ️ This intentionally mimics the interface of {!Stdlib.Lazy}.  Unlike with
      the standard library suspensions an attempt to force a suspension from
      multiple fibers, possibly running on different domains, does not raise the
      {!Undefined} exception. *)

  exception Undefined
  (** Synonym for {!Stdlib.Lazy.Undefined}. *)

  type !'a t
  (** Represents a deferred computation or suspension. *)

  val from_fun : (unit -> 'a) -> 'a t
  (** [from_fun thunk] returns a suspension. *)

  val from_val : 'a -> 'a t
  (** [from_val value] returns an already forced suspension whose result is the
      given [value]. *)

  val is_val : 'a t -> bool
  (** [is_val susp] determines whether the suspension has already been forced
      and didn't raise an exception. *)

  val force : 'a t -> 'a
  (** [force susp] forces the suspension, i.e. computes [thunk ()] using the
      [thunk] passed to {!from_fun}, stores the result of the computation to the
      suspension and reproduces its result.  In case the suspension has already
      been forced the computation is skipped and stored result is reproduced.

      ℹ️ This will check whether the current fiber has been canceled before
      starting the computation of [thunk ()].  This allows the suspension to be
      forced by another fiber.  However, if the fiber is canceled and the
      cancelation exception is raised after the computation has been started,
      the suspension will then store the cancelation exception.

      @raise Undefined in case the suspension is currently being forced by the
        {{!Picos.Fiber.current} current} fiber. *)

  val force_val : 'a t -> 'a
  (** [force_val] is a synonym for {!force}. *)

  val map : ('a -> 'b) -> 'a t -> 'b t
  (** [map fn susp] is equivalent to:
      {[from_fun (fun () -> fn (force susp))]} *)

  val map_val : ('a -> 'b) -> 'a t -> 'b t
  (** [map_val fn susp] is equivalent to:
      {@ocaml skip[
        if is_val susp then
          from_val (fn (force susp))
        else
          map fn susp
      ]} *)
end
