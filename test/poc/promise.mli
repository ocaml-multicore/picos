(** A promise implementation for Picos *)

(** {2 Interface for awaiting} *)

type !'a t
(** *)

val await : 'a t -> 'a
(** *)

val of_computation : ('a, [> `Await | `Cancel ]) Picos.Computation.t -> 'a t
(** *)

val peek : 'a t -> 'a option
(** *)

val both : unit t -> unit t -> unit t
(** *)

val cancel : 'a t -> Picos.Exn_bt.t -> unit
(** *)

module Infix : sig
  (** *)

  val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t
  (** *)

  val ( and+ ) : 'a t -> 'b t -> ('a * 'b) t
  (** *)

  val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
  (** *)

  val ( and* ) : 'a t -> 'b t -> ('a * 'b) t
  (** *)
end

(** {2 Interface for creating} *)

type !'a unpublished
(** *)

val create : unit -> 'a unpublished
(** *)

val return_to : 'a unpublished -> 'a -> unit
(** *)

val reify_to : 'a unpublished -> (unit -> 'a) -> unit
(** *)

val publish : 'a unpublished -> 'a t
(** *)
