type (!'k, !'v) t
(** *)

val create : equal:('k -> 'k -> bool) -> hash:('k -> int) -> unit -> ('k, 'v) t
(** *)

val find_exn : ('k, 'v) t -> 'k -> 'v
(** *)

val try_add : ('k, 'v) t -> 'k -> 'v -> bool
(** *)

val try_remove : ('k, 'v) t -> 'k -> bool
(** *)
