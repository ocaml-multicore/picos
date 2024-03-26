(** A promise implementation for Picos *)

open Picos

(** {2 Interface for awaiting} *)

type !'a t
(** *)

val await : 'a t -> 'a
(** *)

val of_computation : 'a Computation.t -> 'a t
(** *)

val peek : 'a t -> 'a option
(** *)

val both : unit t -> unit t -> unit t
(** *)

val any : 'a t list -> 'a t
(** *)

val try_cancel : 'a t -> Exn_bt.t -> bool
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

val try_return_to : 'a unpublished -> 'a -> bool
(** *)

val try_reify_to : 'a unpublished -> (unit -> 'a) -> bool
(** *)

val publish : 'a unpublished -> 'a t
(** *)
