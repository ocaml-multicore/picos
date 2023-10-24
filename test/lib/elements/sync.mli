(** Mutex and Condition implementations for Picos *)

module Checked : Sync_intf.Mutex
(** *)

(** *)
module Make : functor (Mutex : Sync_intf.Mutex) ->
  Sync_intf.Condition with type mutex := Mutex.t
