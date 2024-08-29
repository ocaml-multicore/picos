(** External reference counting tables for disposable resources. *)

include module type of Intf

(** Creates a new external reference counting table for a resource type. *)
module Make (Resource : Resource) () : S with module Resource = Resource
