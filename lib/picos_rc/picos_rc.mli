(** A reference counting wrapper for disposable resources.

    ℹ️ This is particularly intended for cases where a resource needs to be
    safely shared between multiple independent threads of control whether they
    are fibers, threads, or domains.  In that use case you typically need to
    {{!try_incr} increment} the reference count before handing the resource from
    one independent thread of control to another and the other independent
    thread of control then becomes responsible for {{!decr} decrementing} or
    {{!dispose} disposing} the reference count after being done with the
    resource. *)

type 'a t
(** A reference counting wrapper for a disposable resource of type ['a]. *)

val make : dispose:('a -> unit) -> 'a -> 'a t
(** [make ~dispose resource] creates a new reference counting wrapper for the
    given disposable resource.  The initial reference count will be set to one
    and not marked as to be {{!dispose} disposed}.  Once the reference count
    reaches zero, as a result of {!dispose} or {!decr}, [dispose resource] will
    be called. *)

val unsafe_get : 'a t -> 'a
(** [unsafe_get rc] returns the associated resource.

    ⚠️ This should only be called and the resource used either after {{!make}
    making} the reference counting wrapper and before {{!dispose} disposing} it
    or after {{!try_incr} successfully incrementing} the reference count and
    before the matching {{!decr} decrement}.

    @raise Invalid_argument if the resource has already been disposed. *)

val dispose : 'a t -> unit
(** [dispose rc] atomically checks that the resource has not already been marked
    as to be disposed and has a positive reference count and then marks the
    resource as to be disposed and {{!decr} decrements} the reference count.
    Otherwise does nothing.  In other words, [dispose] is an idempotent version
    of {!decr}. *)

val has_been_disposed : 'a t -> bool
(** [has_been_disposed rc] atomically determines whether the resource has been
    marked as to be disposed.

    The idea is that one can use [has_been_disposed] after having {{!try_incr}
    successfully incremented} the reference count to check or poll whether the
    resource has been concurrently marked as to be disposed. *)

val decr : 'a t -> unit
(** [decr rc] atomically checks that the reference count of the resource is
    positive and decrements it.  If the reference count becomes zero, the
    resource will then be disposed.

    ⚠️ Each successful [try_incr] should be matched by a {!decr}.

    @raise Invalid_argument if the reference count was zero. *)

val try_incr : 'a t -> bool
(** [try_incr rc] atomically checks that the reference count of the resource is
    positive and increments it.  Return [true] on success and [false] when the
    reference count was zero.

    ⚠️ Each successful [try_incr] should be matched by a {!decr}. *)
