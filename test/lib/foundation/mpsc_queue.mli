(** A multi-producer, single-consumer queue *)

type !'a t
(** *)

val create : unit -> 'a t
(** *)

val enqueue : 'a t -> 'a -> unit
(** *)

exception Empty
(** *)

val dequeue : 'a t -> 'a
(** *)
