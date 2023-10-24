module type Mutex = sig
  (** A mutex implementation for Picos *)

  type t
  (** *)

  val create : unit -> t
  (** *)

  val lock : t -> unit
  (** *)

  val try_lock : t -> bool
  (** *)

  val unlock : t -> unit
  (** *)

  val protect : t -> (unit -> 'a) -> 'a
  (** *)

  val succumb : t -> (unit -> 'a) -> 'a
  (** *)
end

module type Condition = sig
  (** A condition implementation for Picos *)

  type mutex
  (** *)

  type t
  (** *)

  val create : unit -> t
  (** *)

  val wait : t -> mutex -> unit
  (** *)

  val signal : t -> unit
  (** *)

  val broadcast : t -> unit
  (** *)
end
