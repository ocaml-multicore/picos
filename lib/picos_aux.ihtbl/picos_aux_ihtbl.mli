type (!'k, !'v) t
type 'k hashed_type = (module Stdlib.Hashtbl.HashedType with type t = 'k)

val create :
  ?hashed_type:'k hashed_type ->
  ?min_buckets:int ->
  ?max_buckets:int ->
  unit ->
  ('k, 'v) t

val find_exn : ('k, 'v) t -> 'k -> 'v
val try_add : ('k, 'v) t -> 'k -> 'v -> bool
val set_exn : ('k, 'v) t -> 'k -> 'v -> 'v
val remove_exn : ('k, 'v) t -> 'k -> 'v
val clear : ('k, 'v) t -> unit
val to_seq : ('k, 'v) t -> ('k * 'v) Seq.t

(**/**)

val non_linearizable_length : ('k, 'v) t -> int
