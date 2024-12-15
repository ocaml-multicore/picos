(** Minimalistic thread API available with or without [threads.posix]. *)

val is_main_thread : unit -> bool
(** [is_main_thread ()] determines whether running on the main thread of the
    application. *)

module TLS : sig
  (** Thread-local storage.

      Note that here "thread" refers to system level threads rather than fibers
      or domains. In case a system level thread implementation, i.e. the
      [threads.posix] library, is not available, this will use
      {!Picos_domain.DLS}. *)

  type 'a t
  (** Represents a key for associating values with threads. *)

  val create : unit -> 'a t
  (** [create ()] allocates a new key for associating values with threads.

      ⚠️ Keys should not be created dynamically as each key will potentially
      increase the space taken by every thread. *)

  exception Not_set
  (** Exception raised by {!get_exn} when no value is associated with the
      specified key for the current thread. *)

  val get_exn : 'a t -> 'a
  (** [get_exn key] returns the value associated with the specified key for the
      current thread or raises {!Not_set} in case no value has been {!set} for
      the key.

      ⚠️ The {!Not_set} exception is raised with no backtrace. Always catch the
      exception unless it is known that a value has been set. *)

  val set : 'a t -> 'a -> unit
  (** [set key value] associates the value with the specified key for the
      current thread. *)
end
