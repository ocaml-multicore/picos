(** *)

open Picos

module Id : sig
  (** *)

  type t
  (** *)

  val compare : t -> t -> int
  (** *)

  val get : unit -> t
  (** *)
end

module Priority : sig
  (** *)

  type t
  (** *)

  val compare : t -> t -> int
  (** *)

  val default : t
  (** *)

  val higher : t -> t
  (** *)

  val get : unit -> t
  (** *)

  val set : t -> unit
  (** *)
end

module Mutex : sig
  (** *)

  type t
  (** *)

  val create : ?padded:bool -> unit -> t
  (** *)

  val lock : t -> unit
  (** *)

  val unlock : t -> unit
  (** *)
end

module Condition : sig
  (** *)

  type t
  (** *)

  val create : ?padded:bool -> unit -> t
  (** *)

  val wait : t -> Mutex.t -> unit
  (** *)

  val broadcast : t -> unit
  (** *)
end

val run_fiber :
  ?fatal_exn_handler:(exn -> unit) -> Fiber.t -> (Fiber.t -> unit) -> unit
(** *)

val run : ?fatal_exn_handler:(exn -> unit) -> ?forbid:bool -> (unit -> 'a) -> 'a
(** *)
