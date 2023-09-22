(** *)

(** *)
module Make : functor (Mutex : Sync_intf.Mutex) ->
  Sync_intf.Condition with type mutex := Mutex.t
