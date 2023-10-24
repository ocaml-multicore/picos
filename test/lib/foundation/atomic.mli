include module type of Stdlib.Atomic

val update : 'a t -> ('a -> 'a) -> 'a
(** *)

val modify : 'a t -> ('a -> 'a) -> unit
(** *)
