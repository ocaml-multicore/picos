(** Lock-free hash table.

    The operations provided by this hash table are designed to work as building
    blocks of non-blocking algorithms.  Specifically, the operation signatures
    and semantics are designed to allow building
    {{:https://dl.acm.org/doi/10.1145/62546.62593} consensus protocols over
    arbitrary numbers of processes}.

    🏎️ Single key reads with this hash table are actually wait-free rather than
    just lock-free.  Internal resizing automatically uses all the threads that
    are trying to write to the hash table. *)

(** {1 API} *)

type (!'k, !'v) t
(** Represents a lock-free hash table mapping keys of type ['k] to values of
    type ['v]. *)

type 'k hashed_type = (module Stdlib.Hashtbl.HashedType with type t = 'k)
(** First-class module type abbreviation. *)

val create :
  ?hashed_type:'k hashed_type ->
  ?min_buckets:int ->
  ?max_buckets:int ->
  unit ->
  ('k, 'v) t
(** [create ~hashed_type:(module Key) ()] creates a new empty lock-free hash
    table.

    - The optional [hashed_type] argument can and usually should be used to
      specify the [equal] and [hash] operations on keys.  Slow polymorphic
      equality [(=)] and slow polymorphic {{!Stdlib.Hashtbl.seeded_hash} [seeded_hash (Bits64.to_int (Random.bits64 ()))]}
      is used by default.
    - The default [min_buckets] is unspecified and a given [min_buckets] may be
      adjusted by the implementation.
    - The default [max_buckets] is unspecified and a given [max_buckets] may be
      adjusted by the implementation. *)

val hashed_type_of : ('k, 'v) t -> 'k hashed_type
(** [hashed_type_of htbl] returns a copy of the hashed type used when the hash
    table [htbl] was created. *)

val min_buckets_of : ('k, 'v) t -> int
(** [min_buckets_of htbl] returns the minimum number of buckets of the hash
    table [htbl].

    ℹ️ The returned value may not be the same as given to {!create}. *)

val max_buckets_of : ('k, 'v) t -> int
(** [max_buckets_of htbl] returns the maximum number of buckets of the hash
    table [htbl].

    ℹ️ The returned value may not be the same as given to {!create}. *)

(** {2 Looking up bindings} *)

val find_exn : ('k, 'v) t -> 'k -> 'v
(** [find_exn htbl key] returns the current binding of [key] in the hash table
    [htbl] or raises {!Not_found} if no such binding exists.

    @raise Not_found in case no binding of [key] exists in the hash table
      [htbl]. *)

val mem : ('k, 'v) t -> 'k -> bool
(** [mem htbl key] determines whether the hash table [htbl] has a binding for
    the [key]. *)

(** {2 Adding bindings} *)

val try_add : ('k, 'v) t -> 'k -> 'v -> bool
(** [try_add htbl key value] tries to add a new binding of [key] to [value] to
    the hash table [htbl].  Returns [true] on success and [false] in case the
    hash table already contained a binding for [key]. *)

(** {2 Updating bindings} *)

val try_set : ('k, 'v) t -> 'k -> 'v -> bool
(** [try_set htbl key value] tries to update an existing binding of [key] to
    [value] in the hash table [htbl].  Returns [true] on success and [false] in
    case the hash table did not contain a binding for [key]. *)

val try_compare_and_set : ('k, 'v) t -> 'k -> 'v -> 'v -> bool
(** [try_compare_and_set htbl key before after] tries to update an existing
    binding of [key] from the [before] value to the [after] value in the hash
    table [htbl].  Returns [true] on success and [false] in case the hash table
    did not contain a binding of [key] to the [before] value.

    ℹ️ The values are compared using physical equality, i.e. the [==]
    operator. *)

val set_exn : ('k, 'v) t -> 'k -> 'v -> 'v
(** [set_exn htbl key after] tries to update an existing binding of [key] from
    some [before] value to the [after] value in the hash table [htbl].  Returns
    the [before] value on success or raises {!Not_found} if no such binding
    exists.

    @raise Not_found in case no binding of [key] exists in the hash table
      [htbl]. *)

(** {2 Removing bindings} *)

val try_remove : ('k, 'v) t -> 'k -> bool
(** [try_remove htbl key] tries to remove a binding of [key] from the hash table
    [htbl].  Returns [true] on success and [false] in case the hash table did
    not contain a binding for [key]. *)

val try_compare_and_remove : ('k, 'v) t -> 'k -> 'v -> bool
(** [try_compare_and_remove htbl key before] tries to remove a binding of [key]
    to the [before] value from the hash table [htbl].  Returns [true] on success
    and [false] in case the hash table did not contain a binding of [key] to the
    [before] value.

    ℹ️ The values are compared using physical equality, i.e. the [==]
    operator. *)

val remove_exn : ('k, 'v) t -> 'k -> 'v
(** [remove_exn htbl key] tries to remove a binding of [key] to some [before]
    value from the hash table [htbl].  Returns the [before] value on success or
    raises {!Not_found} if no such binding exists.

    @raise Not_found in case no binding of [key] exists in the hash table
      [htbl]. *)

(** {2 Examining contents} *)

val to_seq : ('k, 'v) t -> ('k * 'v) Seq.t
(** [to_seq htbl] takes a snapshot of the bindings in the hash table [htbl] and
    returns them as an association sequence.

    🐌 This is a linear time operation. *)

val remove_all : ('k, 'v) t -> ('k * 'v) Seq.t
(** [remove_all htbl] takes a snapshot of the bindings in the hash table [htbl],
    removes the bindings from the hash table, and returns the snapshot as an
    association sequence.

    🐌 This is a linear time operation. *)

val find_random_exn : ('k, 'v) t -> 'k
(** [find_random_exn htbl] tries to find a random binding from the hash table
    [htbl] and returns the key of the binding or raises {!Not_found} in case the
    hash table is empty.

    🐌 This is an expected constant time operation with worst case linear time
    complexity.

    @raise Not_found in case the hash table [htbl] is empty. *)

(** {1 Examples}

    For the examples we first make a convenience binding:

    {[
      module Htbl = Picos_aux_htbl
    ]}

    {2 A simple top-level session}

    Here is a top-level session using a hash table:

    {[
      # let t : (int, string) Htbl.t =
          Htbl.create
            ~hashed_type:(module Int) ()
      val t : (int, string) Htbl.t = <abstr>

      # Htbl.try_add t 42 "The answer"
      - : bool = true

      # Htbl.try_add t 101 "Basics"
      - : bool = true

      # Htbl.find_exn t 42
      - : string = "The answer"

      # Htbl.try_add t 101 "The basics"
      - : bool = false

      # Htbl.remove_all t |> List.of_seq
      - : (int * string) list = [(101, "Basics"); (42, "The answer")]
    ]}

    {2 A randomized lock-free bag}

    Below is an example of a randomized lock-free bag implemented using a hash
    table:

    {[
      module Bag : sig
        type !'v t

        val create : unit -> 'v t
        val push : 'v t -> 'v -> unit
        val pop_exn : 'v t -> 'v
      end = struct
        type 'v t = (int, 'v) Htbl.t

        module Key = struct
          type t = int
          let equal = Int.equal
          let hash = Fun.id
        end

        let create () =
          Htbl.create ~hashed_type:(module Key) ()

        let rec push t value =
          let key = Int64.to_int (Random.bits64 ()) in
          if not (Htbl.try_add t key value) then
            push t value

        let rec pop_exn t =
          let key = Htbl.find_random_exn t in
          try
            Htbl.remove_exn t key
          with Not_found ->
            pop_exn t
      end
    ]}

    First of all, as we use random bits as keys, we can use {!Fun.id} as the
    [hash] function.  However, the main idea demonstrated above is that the
    {!try_add} and {!remove_exn} operations can be used as building blocks of
    lock-free algorithms. *)
