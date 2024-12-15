(** Basic event abstraction for {!Picos}. *)

module Event : sig
  (** First-class synchronous communication abstraction.

      Events describe a thing that might happen in the future, or a concurrent
      offer or request that might be accepted or succeed, but is cancelable if
      some other event happens first.

      See the {!Picos_io_select} library for an example.

      ℹ️ This module intentionally mimics the
      {{:https://ocaml.org/manual/5.2/api/Event.html} [Event]} module provided
      by the OCaml POSIX threads library. *)

  type !'a t
  (** An event returning a value of type ['a]. *)

  type 'a event = 'a t
  (** An alias for the {!Event.t} type to match the
      {{:https://ocaml.org/manual/5.2/api/Event.html} [Event]} module signature.
  *)

  val always : 'a -> 'a t
  (** [always value] returns an event that can always be committed to resulting
      in the given [value]. *)

  (** {2 Composing events} *)

  val choose : 'a t list -> 'a t
  (** [choose events] return an event that offers all of the given events and
      then commits to at most one of them. *)

  val wrap : 'b t -> ('b -> 'a) -> 'a t
  (** [wrap event fn] returns an event that acts as the given [event] and then
      applies the given function to the value in case the event is committed to.
  *)

  val map : ('b -> 'a) -> 'b t -> 'a t
  (** [map fn event] is equivalent to {{!wrap} [wrap event fn]}. *)

  val guard : (unit -> 'a t) -> 'a t
  (** [guard thunk] returns an event that, when {{!sync} synchronized}, calls
      the [thunk], and then behaves like the resulting event.

      ⚠️ Raising an exception from a [guard thunk] will result in raising that
      exception out of the {!sync}. This may result in dropping the result of an
      event that committed just after the exception was raised. This means that
      you should treat an unexpected exception raised from {!sync} as a fatal
      error. *)

  (** {2 Consuming events} *)

  val sync : 'a t -> 'a
  (** [sync event] synchronizes on the given event.

      Synchronizing on an event executes in three phases:

      + In the first phase offers or requests are made to communicate.
      + One of the offers or requests is committed to and all the other offers
        and requests are canceled.
      + A final result is computed from the value produced by the event.

      ⚠️ [sync event] does not wait for the canceled concurrent requests to
      terminate. This means that you should arrange for guaranteed cleanup
      through other means such as the use of
      {{!Picos_std_structured} structured concurrency}. *)

  val select : 'a t list -> 'a
  (** [select events] is equivalent to {{!sync} [sync (choose events)]}. *)

  (** {2 Primitive events}

      ℹ️ The {{!Picos.Computation} [Computation]} concept of {!Picos} can be seen
      as a basic single-shot atomic event. This module builds on that concept to
      provide a composable API to concurrent services exposed through
      computations. *)

  open Picos

  type 'a request = {
    request : 'r. (unit -> 'r) Computation.t -> ('a -> 'r) -> unit;
  }
  [@@unboxed]
  (** Represents a function that requests a concurrent service to update a
      {{!Picos.Computation} computation}.

      ℹ️ The computation passed to a request may be completed by some other event
      at any point. All primitive requests should be implemented carefully to
      take that into account. If the computation is completed by some other
      event, then the request should be considered as canceled, take no effect,
      and not leak any resources.

      ⚠️ Raising an exception from a [request] function will result in raising
      that exception out of the {!sync}. This may result in dropping the result
      of an event that committed just after the exception was raised. This means
      that you should treat an unexpected exception raised from {!sync} as a
      fatal error. In addition, you should arrange for concurrent services to
      report unexpected errors independently of the computation being passed to
      the service. *)

  val from_request : 'a request -> 'a t
  (** [from_request { request }] creates an {{!Event} event} from the request
      function. *)

  val from_computation : 'a Computation.t -> 'a t
  (** [from_computation source] creates an {{!Event} event} that can be
      committed to once the given [source] computation has completed.

      ℹ️ Committing to some other event does not cancel the [source] computation.
  *)
end
