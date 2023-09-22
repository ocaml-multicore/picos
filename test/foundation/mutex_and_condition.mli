type t
(** *)

val get : unit -> t
(** *)

val lock : t -> unit
(** *)

val unlock : t -> unit
(** *)

val broadcast : t -> unit
(** *)

val wait : t -> unit
(** *)
