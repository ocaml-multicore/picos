module type Resource = sig
  (** A resource that must be explicitly {{!val-dispose} disposed}. *)

  type t
  (** Represents a disposable resource. *)

  val equal : t -> t -> bool
  (** [equal resource1 resource2] determines whether [resource1] and [resource2]
      are one and the same. *)

  val hash : t -> int
  (** [hash resource] computes the hash value for [resource]. *)

  val dispose : t -> unit
  (** [dispose resource] releases the resource.

      ⚠️ The physical [resource] value may be reused only after [dispose] has
      been called on it. *)
end

module type S = sig
  (** An externally reference counted resource.

      ℹ️ This is intended for cases where a resource needs to be safely shared
      between multiple independent threads of control whether they are fibers,
      threads, or domains. In that use case you typically need to
      {{!incr} increment} the reference count before handing the resource from
      one independent thread of control to another and the other independent
      thread of control then becomes responsible for {{!decr} decrementing} the
      reference count after being done with the resource. *)

  module Resource : Resource
  (** Resource type. *)

  type t
  (** Opaque type alias for the resource type. *)

  val create : ?dispose:bool -> Resource.t -> t
  (** [create resource] adds an entry for the resource with an initial reference
      count of [1] to the table for the resource and returns the resource as a
      value of the {{!t} opaque alias type}.

      The optional [dispose] argument defaults to [true]. When explicitly
      specified as [~dispose:false], the resource will not be
      {{!module-Resource.dispose} disposed} when the reference count becomes
      zero. This is intended for special cases where a resource is e.g. managed
      outside of the control of the user program. *)

  val unsafe_get : t -> Resource.t
  (** [unsafe_get opaque_resource] casts the opaque alias type back to the
      resource type.

      ⚠️ This should only be called and the resource used either after
      {{!create} creating} the reference counting entry or after
      {{!incr} incrementing} the reference count and before the matching
      {{!decr} decrement}. *)

  val incr : t -> unit
  (** [incr opaque_resource] tries to find the entry for the resource and
      increment the reference count.

      @raise Invalid_argument
        in case no entry is found for the resource or the reference count was
        zero or the resource was marked as closed previously by a
        {{!decr} decrement} operation. *)

  val decr : ?close:bool -> t -> unit
  (** [decr opaque_resource] tries to find the entry for the resource and
      decrement the reference count. If the reference count becomes zero, the
      entry for the resource will be removed and the resource will be
      {{!module-Resource.dispose} disposed}, unless [~dispose:false] was
      specified for {!create}.

      The optional [close] argument defaults to [false]. When explicitly
      specified as [~close:true] the resource will be marked as closed and
      attempts to {{!incr} increment} the reference will fail.

      @raise Invalid_argument
        in case no entry is found for the resource or the reference count was
        zero. *)

  type info = {
    resource : Resource.t;  (** The resource. *)
    count : int;  (** Reference count. This may be [0]. *)
    closed : bool;  (** Whether the resource has been closed, see {!decr}. *)
    dispose : bool;  (** Whether to dispose the resource, see {!create}. *)
    bt : Printexc.raw_backtrace;  (** Backtrace captured at {!create}. *)
  }
  (** Information on a resource. *)

  val infos : unit -> info Seq.t
  (** [infos ()] returns a sequence of entries in the reference counting table
      at the point in time of of calling [infos ()]. *)
end
