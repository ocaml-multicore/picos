(** A per thread {!Mutex} and {!Condition} for {!Picos}. *)

type t
(** Pair of a {!Mutex} and a {!Condition} for a thread. *)

val get : unit -> t
(** [get ()] obtains the pair of a {!Mutex} and a {!Condition} for the current
    thread. *)

val lock : t -> unit
(** [lock ptmc] locks the {!Mutex} of the [ptmc]. *)

val unlock : t -> unit
(** [unlock ptmc] unlocks the {!Mutex} of the [ptmc]. *)

val broadcast : t -> unit
(** [broadcast ptmc] locks and unlocks the {!Mutex} of the [ptmc] and then
    broadcasts on the {!Condition} of the [ptmc]. *)

val wait : t -> unit
(** [wait ptmc] waits on the {!Condition} of the [ptmc]. *)
