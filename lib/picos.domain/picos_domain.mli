(** Minimalistic domain API available both on OCaml 5 and on OCaml 4.

    ℹ️ On OCaml 4 there is always only a single domain. *)

val at_exit : (unit -> unit) -> unit
(** [at_exit action] registers [action] to be called when the current domain
    exits.

    On OCaml 5 this calls {!Domain.at_exit}. On OCaml 4 this calls
    {!Stdlib.at_exit}. *)

val recommended_domain_count : unit -> int
(** [recommended_domain_count ()] returns [1] on OCaml 4 and calls
    {!Domain.recommended_domain_count} on OCaml 5. *)

val is_main_domain : unit -> bool
(** [is_main_domain ()] returns [true] on OCaml 4 and calls
    {!Domain.is_main_domain} on OCaml 5. *)

module DLS : sig
  (** Domain-local storage for Picos.

      ℹ️ On OCaml 4 there is always only a single domain. *)

  type 'a key
  (** Represents a key for storing values of type ['a] in storage associated
      with domains. *)

  val new_key : (unit -> 'a) -> 'a key
  (** [new_key compute] allocates a new key for associating values in storage
      associated with domains. The initial value for each domain is [compute]d
      by calling the given function if the [key] is {{!get} read} before it has
      been {{!set} written}. The [compute] function might be called multiple
      times per domain, but only one result will be used. *)

  val get : 'a key -> 'a
  (** [get key] returns the value associated with the [key] in the storage
      associated with the current domain. *)

  val set : 'a key -> 'a -> unit
  (** [set key value] sets the [value] associated with the [key] in the storage
      associated with the current domain. *)
end
