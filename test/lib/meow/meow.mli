module Ownership : sig
  type t

  val create : finally:('a -> unit) -> 'a -> t
  val own : t -> unit
  val check : t -> unit
  val disown : t -> unit
  val bless : t -> unit
end

module Promise : sig
  type !'a t

  val async : ?give:Ownership.t list -> (unit -> 'a) -> 'a t
  val await : 'a t -> 'a
end
