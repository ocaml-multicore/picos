(** Minimalistic thread API available with or without [threads.posix]. *)

val is_main_thread : unit -> bool
(** [is_main_thread ()] determines whether running on the main thread of the
    application. *)

module TLS : sig
  (** Thread-local storage.

      Note that here "thread" refers to system level threads rather than fibers
      or domains.  In case a system level thread implementation, i.e. the
      [threads.posix] library, is not available, this will use
      {!Picos_domain.DLS}. *)

  type 'a key
  (** Represents a key for storing values of type ['a] in storage associated
      with threads. *)

  val new_key : (unit -> 'a) -> 'a key
  (** [new_key compute] allocates a new key for associating values in storage
      associated with threads.  The initial value for each thread is [compute]d
      by calling the given function if the [key] is {{!get}read} before it has
      been {{!set}written}. *)

  val get : 'a key -> 'a
  (** [get key] returns the value associated with the [key] in the storage
      associated with the current thread. *)

  val set : 'a key -> 'a -> unit
  (** [set key value] sets the [value] associated with the [key] in the storage
      associated with the current thread. *)
end
