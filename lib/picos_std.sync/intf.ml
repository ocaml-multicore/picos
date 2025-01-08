module type Condition = sig
  type mutex
  (** Represents a mutual exclusion lock. *)

  type t
  (** Represents a condition variable *)

  val create : ?padded:bool -> unit -> t
  (** [create ()] return a new condition variable. *)

  val wait : t -> mutex -> unit
  (** [wait condition mutex] unlocks the [mutex], waits for the [condition], and
      locks the [mutex] before returning or raising due to the operation being
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
