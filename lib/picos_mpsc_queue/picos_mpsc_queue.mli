(** Lock-free multi-producer, single-consumer queue.

    🏎️ This data structure is optimized for use as a scheduler's ready queue. *)

(** {1 API} *)

type !'a t
(** A multi-producer, single-consumer queue. *)

val create : unit -> 'a t
(** [create ()] returns a new empty multi-producer, single-consumer queue. *)

(** {2 Interface for producers}

    ℹ️ The operations in this section can be called by both any number of
    producers and the single owner / consumer of the queue. *)

val push : 'a t -> 'a -> unit
(** [push queue value] adds the [value] to the tail of the [queue]. *)

val push_head : 'a t -> 'a -> unit
(** [push_head queue value] adds the [value] to the head of the [queue]. *)

(** {2 Interface for the owner / consumer}

    ⚠️ The operations in this section should only be called by the single owner /
    consumer of the queue. *)

exception Empty
(** Raised by {!pop_exn} in case it finds the queue empty. *)

val pop_exn : 'a t -> 'a
(** [pop_exn queue] tries to remove the value at the head of the [queue].
    Returns the removed value or raises {!Empty} in case the queue was empty.

    @raise Empty in case the queue was empty. *)

val pop_all : 'a t -> 'a Seq.t
(** [pop_all queue] removes all values from the [queue] and returns them as a
    sequence. *)

(** {1 Examples}

    An example top-level session:
    {[
      # let q : int Picos_mpsc_queue.t =
          Picos_mpsc_queue.create ()
      val q : int Picos_mpsc_queue.t = <abstr>

      # Picos_mpsc_queue.push q 42
      - : unit = ()

      # Picos_mpsc_queue.push_head q 76
      - : unit = ()

      # Picos_mpsc_queue.push q 101
      - : unit = ()

      # Picos_mpsc_queue.pop_exn q
      - : int = 76

      # Picos_mpsc_queue.pop_all q |> List.of_seq
      - : int list = [42; 101]
    ]} *)
