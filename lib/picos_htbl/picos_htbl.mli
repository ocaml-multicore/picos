(** Lock-free hash table.

    🏎️ Single key reads with this hash table are actually wait-free rather than
    just lock-free.  Internal resizing automatically uses all the threads that
    are trying to write to the hash table. *)

(** {1 API} *)

type (!'k, !'v) t
(** Represents a lock-free hash table mapping keys of type ['k] to values of
    type ['v]. *)

type 'k hashed_type = (module Stdlib.Hashtbl.HashedType with type t = 'k)
(** First-class module type abbreviation. *)

val create : ?hashed_type:'k hashed_type -> unit -> ('k, 'v) t
(** [create ~hashed_type:(module Key) ()] creates a new empty lock-free hash
    table.

    The optional [hashed_type] argument can be used to specify the [equal] and
    [hash] operations on keys.  Slow polymorphic equality [(=)] and slow
    polymorphic {{!Stdlib.Hashtbl.seeded_hash} [seeded_hash (Random.bits ())]}
    is used by default. *)

(** {2 Looking up bindings} *)

val find_exn : ('k, 'v) t -> 'k -> 'v
(** [find_exn htbl key] returns the current binding of [key] in the hash table
    [htbl] or raises {!Not_found} if no such binding exists. *)

val mem : ('k, 'v) t -> 'k -> bool
(** [mem htbl key] determines whether the hash table [htbl] has a binding for
    the [key]. *)

val to_seq : ('k, 'v) t -> ('k * 'v) Seq.t
(** [to_seq htbl] takes a snapshot of the bindings in the hash table and returns
    them as an association sequence.

    🐌 This is a linear time operation. *)

val find_random_exn : ('k, 'v) t -> 'k
(** [find_random_exn htbl] tries to find a random binding from the hash table
    and returns the key of the binding or raises {!Not_found} in case the hash
    table is empty.

    🐌 This is an expected constant time operation with worst case linear time
    complexity. *)

(** {2 Adding bindings} *)

val try_add : ('k, 'v) t -> 'k -> 'v -> bool
(** [try_add htbl key value] tries to add a new binding of [key] to [value] to
    the hash table [htbl]. Returns [true] on success and [false] in case the
    hash table already contained a binding for [key]. *)

(** {2 Removing bindings} *)

val try_remove : ('k, 'v) t -> 'k -> bool
(** [try_remove htbl key] tries to remove a binding of [key] from the hash table
    [htbl].  Returns [true] on success and [false] in case the hash table did
    not contain a binding for [key]. *)

val remove_exn : ('k, 'v) t -> 'k -> 'v
(** [remove_exn htbl key] tries to remove a binding of [key] from the hash table
    [htbl] and return it or raise {!Not_found} if no such binding exists. *)

val remove_all : ('k, 'v) t -> ('k * 'v) Seq.t
(** [remove_all htbl] takes a snapshot of the bindings in the hash table,
    removes the bindings from the hash table, and returns the snapshot as an
    association sequence.

    🐌 This is a linear time operation. *)

(** {1 Examples}

    An example top-level session:
    {[
      # let t : (int, string) Picos_htbl.t =
          Picos_htbl.create
            ~hashed_type:(module Int) ()
      val t : (int, string) Picos_htbl.t = <abstr>

      # Picos_htbl.try_add t 42 "The answer"
      - : bool = true

      # Picos_htbl.try_add t 101 "Basics"
      - : bool = true

      # Picos_htbl.find_exn t 42
      - : string = "The answer"

      # Picos_htbl.try_add t 101 "The basics"
      - : bool = false

      # Picos_htbl.remove_all t |> List.of_seq
      - : (int * string) list = [(101, "Basics"); (42, "The answer")]
    ]} *)
