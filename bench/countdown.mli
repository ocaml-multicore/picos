(** Scalable low-level countdown. *)

type t
(** Represents a countdown counter. *)

val create : n_domains:int -> unit -> t
(** [create ~n_domains ()] returns a new countdown counter with initial value of
    [0]. *)

val non_atomic_set : t -> int -> unit
(** [non_atomic_set countdown count] sets the [count] of the [countdown].

    ⚠️ This operation is not atomic.  However, it is safe to call
    [non_atomic_set] with the same [countdown] and [count] in parallel, because
    the [countdown] will be initialized deterministically. *)

val get : t -> int
(** [get countdown] returns the count of the [countdown]. *)

val alloc : t -> domain_index:int -> batch:int -> int
(** [alloc countdown ~domain_index ~batch] tries to reduce the count of the
    [countdown] by at most [batch] (which must be positive) and returns the
    number by which the count was reduced or [0] in case the count was already
    [0]. *)
