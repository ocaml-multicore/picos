(** Multi-producer, single-consumer queue.

    🏎️ This data structure is optimized for use as a scheduler's ready queue. *)

type !'a t
(** A multi-producer, single-consumer queue. *)

val create : unit -> 'a t
(** [create ()] returns a new empty multi-producer, single-consumer queue. *)

(** {2 Interface for producers} *)

val push : 'a t -> 'a -> unit
(** [push queue value] adds the [value] to the tail of the [queue]. *)

val push_head : 'a t -> 'a -> unit
(** [push_head queue value] adds the [value] to the head of the [queue]. *)

(** {2 Interface for the owner / consumer} *)

exception Empty
(** Raised by {!pop_exn} in case it finds the queue empty. *)

val pop_exn : 'a t -> 'a
(** [pop_exn queue] tries to remove the value at the head of the [queue].
    Returns the removed value or raises {!Empty} in case the queue was empty.

    ⚠️ This should only be called by the owner / consumer of the queue.

    @raise Empty in case the queue was empty. *)

val pop_all : 'a t -> 'a Seq.t
(** [pop_all queue] removes all values from the [queue] and returns them as a
    sequence. *)
