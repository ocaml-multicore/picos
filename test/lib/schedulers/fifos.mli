(** A simple FIFO scheduler for Picos *)

val run : ?on_unhandled:unit Try_handle.t -> forbid:bool -> (unit -> 'a) -> 'a
(** *)
