module type Condition = sig
  type lock
  (** Represents a lock. *)

  type t
  (** Represents a condition variable *)

  val create : ?padded:bool -> unit -> t
  (** [create ()] return a new condition variable. *)

  val signal : t -> unit
  (** [signal condition] wakes up one fiber waiting on the [condition] variable
      unless there are no such fibers. *)

  val broadcast : t -> unit
  (** [broadcast condition] wakes up all the fibers waiting on the [condition]
      variable. *)

  val wait : t -> lock -> unit
  (** [wait condition lock] releases the [lock], waits for the [condition], and
      acquires the [lock] before returning or raising due to the operation being
      canceled.

      ⚠️ If the fiber has been canceled or is canceled during {!wait} and
      propagation of cancelation is allowed, this may raise the cancelation
      exception, which can make an enclosing lock {!holding} operation of a
      poisonable lock to {!poison} the lock. If you do not want the lock to be
      poisoned in case of cancelation, then you need to either use {!protect} on
      the lock or handle the cancelation exception.

      ℹ️ If the lock supports poisoning and the lock is {{!poison} poisoned}
      during the {!wait}, then the {!Poisoned} exception will be raised.

      ℹ️ If the lock supports freezing and the lock is {{!freeze} frozen} during
      the {!wait}, then the {!Frozen} exception will be raised. *)
end
