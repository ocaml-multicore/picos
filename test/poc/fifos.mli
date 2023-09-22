(** A simple FIFO scheduler for Picos *)

val run : forbid:bool -> (unit -> 'a) -> 'a
(** *)
