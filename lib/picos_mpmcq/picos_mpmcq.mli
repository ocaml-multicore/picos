(** Lock-free multi-producer, multi-consumer queue.

    🏎️ This data structure is optimized for use as the ready queue of a fair
    (i.e. FIFO) multi-threaded scheduler. *)

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

(** {1 Examples}

    An example top-level session:
    {[
      # let q : int Picos_mpmcq.t =
          Picos_mpmcq.create ()
      val q : int Picos_mpmcq.t = <abstr>

      # Picos_mpmcq.push q 42
      - : unit = ()

      # Picos_mpmcq.push_head q 76
      - : unit = ()

      # Picos_mpmcq.push q 101
      - : unit = ()

      # Picos_mpmcq.pop_exn q
      - : int = 76

      # Picos_mpmcq.pop_exn q
      - : int = 42

      # Picos_mpmcq.pop_exn q
      - : int = 101

      # Picos_mpmcq.pop_exn q
      Exception: Picos_mpmcq.Empty.
    ]} *)
