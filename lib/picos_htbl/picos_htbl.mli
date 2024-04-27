(** Lock-free hash table.

    Example:
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
    ]}

    🏎️ Single key reads with this hash table are actually wait-free rather than
    just lock-free.  Internal resizing automatically uses all the threads that
    are trying to write to the hash table. *)

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

val find_exn : ('k, 'v) t -> 'k -> 'v
(** [find_exn htbl key] returns the current binding of [key] in the hash table
    [htbl] or raises {!Not_found} if no such binding exists. *)

val mem : ('k, 'v) t -> 'k -> bool
(** [mem htbl key] determines whether the hash table [htbl] has a binding for
    the [key]. *)

val try_add : ('k, 'v) t -> 'k -> 'v -> bool
(** [try_add htbl key value] tries to add a new binding of [key] to [value] to
    the hash table [htbl]. Returns [true] on success and [false] in case the
    hash table already contained a binding for [key]. *)

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

val to_seq : ('k, 'v) t -> ('k * 'v) Seq.t
(** [to_seq htbl] takes a snapshot of the bindings in the hash table and returns
    them as an association sequence.

    🐌 This is a linear time operation. *)
