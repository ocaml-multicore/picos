(** A library for concurrent-ML style alternatives.

   Alternatives provide a compositional mechanism to wait for multiple
   possible events to happen, and act based on which one happened
   first. *)

module Event : sig
  type 'a t
  (** An event returning a value of type ['a]. Events describe
      a thing that might happen in the future, or an action that might
      succeed but is cancellable if some other event happened first. *)

  exception Cancel
  (** Exception used to cancel a branch not taken *)

  val timeout : seconds:float -> unit t

  val make :
    subscribe:(int Picos.Computation.t -> int -> [ `ok | `already_happened ]) ->
    resolve:(unit -> 'a) ->
    unit ->
    'a t
end

module Branch : sig
  type 'a t
  (** A branch that, when followed, returns a value of type ['a].

      A branch is a pair of an event, and a handler for this event.
      The handler is called with the result of the event if the event
      was successfully resolved. *)

  val make : 'a Event.t -> ('a -> 'res) -> 'res t
end

val select : 'a Branch.t list -> 'a
(** [select branches] picks the first branch whose event was resolved. *)
