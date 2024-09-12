val run : (unit -> unit) -> unit
(** *)

module Pid : sig
  (** *)

  type t
  (** *)
end

val spawn : (unit -> unit) -> Pid.t
(** *)

val self : unit -> Pid.t
(** *)

val wait : Pid.t -> unit
(** *)

module Message : sig
  (** *)

  type t = ..
  (** *)
end

val receive : unit -> Message.t
(** *)

val send : Pid.t -> Message.t -> unit
(** *)

type Message.t += Terminated of Pid.t  (** *)

val monitor : at:Pid.t -> the:Pid.t -> unit
(** *)

exception Terminate
(** *)

val link : Pid.t -> Pid.t -> unit
(** *)
