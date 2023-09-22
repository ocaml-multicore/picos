(** A lazy implementation for Picos *)

type !'a t
(** *)

val from_fun : (unit -> 'a) -> 'a t
(** *)

val from_val : 'a -> 'a t
(** *)

val force : 'a t -> 'a
(** *)

val map : ('a -> 'b) -> 'a t -> 'b t
(** *)
