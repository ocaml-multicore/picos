type !'a t
type !'a node

val create : unit -> 'a t
val is_empty : 'a t -> bool
val new_node : 'a -> 'a node
val value : 'a node -> 'a
val remove : 'a node -> unit
val move_l : 'a t -> 'a node -> unit
val iter_l : ('a -> unit) -> 'a t -> unit
