(** An implementation of structured concurrency for Picos *)

type t
(** *)

val run : (t -> 'a) -> 'a
(** *)

val fork : t -> (unit -> 'a) -> 'a Promise.t
(** *)
