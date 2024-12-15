(** Lock-free multi-producer, multi-consumer queue.

    🏎️ This data structure is optimized for use as a building block of the ready
    queue of a (mostly) fair (i.e. mostly FIFO) multi-threaded scheduler. For
    example, one could use a queue per thread, to reduce contention, and have
    threads attempt to pop fibers from the queues of other threads when their
    local queues are empty. It is also possible to use only a single shared
    queue, but that will result in very high contention as this queue is not
    relaxed. *)

(** {1 API} *)

type !'a t
(** A multi-producer, multi-consumer queue. *)

val create : ?padded:bool -> unit -> 'a t
(** [create ()] returns a new empty multi-producer, multi-consumer queue. *)

val push : 'a t -> 'a -> unit
(** [push queue value] adds the [value] to the tail of the [queue]. *)

val push_head : 'a t -> 'a -> unit
(** [push_head queue value] adds the [value] to the head of the [queue]. *)

exception Empty
(** Raised by {!pop_exn} in case it finds the queue empty. *)

val pop_exn : 'a t -> 'a
(** [pop_exn queue] tries to remove the value at the head of the [queue].
    Returns the removed value or raises {!Empty} in case the queue was empty.

    @raise Empty in case the queue was empty. *)

val length : 'a t -> int
(** [length queue] returns the length or the number of values in the [queue]. *)

val is_empty : 'a t -> bool
(** [is_empty queue] is equivalent to {{!length} [length queue = 0]}. *)

(** {1 Examples}

    An example top-level session:
    {[
      # let q : int Picos_aux_mpmcq.t =
          Picos_aux_mpmcq.create ()
      val q : int Picos_aux_mpmcq.t = <abstr>

      # Picos_aux_mpmcq.push q 42
      - : unit = ()

      # Picos_aux_mpmcq.push_head q 76
      - : unit = ()

      # Picos_aux_mpmcq.length q
      - : int = 2

      # Picos_aux_mpmcq.push q 101
      - : unit = ()

      # Picos_aux_mpmcq.pop_exn q
      - : int = 76

      # Picos_aux_mpmcq.pop_exn q
      - : int = 42

      # Picos_aux_mpmcq.pop_exn q
      - : int = 101

      # Picos_aux_mpmcq.pop_exn q
      Exception: Picos_aux_mpmcq.Empty.
    ]} *)
