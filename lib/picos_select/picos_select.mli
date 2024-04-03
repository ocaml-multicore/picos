(** Basic {!Unix.select} based IO event loop for {!Picos}.

    The operations in this module automatically manage a {!Thread} per domain
    that runs a {!Unix.select} loop to support the operations.

    ⚠️ All the usual limitations of the {!Unix} module apply. *)

open Picos

(** {1 Timeouts} *)

val cancel_after : _ Computation.t -> seconds:float -> Exn_bt.t -> unit
(** [cancel_after computation ~seconds exn_bt] arranges for [computation] to be
    {{!Picos.Computation.cancel} canceled} with given [exn_bt] after given time
    in [seconds].  Completion of the [computation] before the specified time
    effectively cancels the timeout.

    ℹ️ You can use [cancel_after] to implement the handler for the
    {{!Picos.Computation.Cancel_after} [Cancel_after]} effect. *)

(** {1 IO} *)

val return_on : 'a Computation.t -> Picos_fd.t -> [ `R | `W | `E ] -> 'a -> unit
(** [return_on computation fd op value] arranges for [computation] to be
    {{!Picos.Computation.return} returned} with given [value] when [fd] becomes
    available for [op].  Completion of the [computation] before the [fd] becomes
    available for [op] effectively cancels the await.

    ℹ️ Using {!Unix.set_nonblock} and [return_on] you can implement direct-style
    transparently asynchronous IO on top of the {!Unix} module. *)

val await_on : Picos_fd.t -> [ `R | `W | `E ] -> Picos_fd.t
(** [await_on fd op] awaits until [fd] becomes available for [op]. *)

module Intr : sig
  (** A mechanism to interrupt blocking {!Unix} IO operations.

      ⚠️ The mechanism uses the {!Sys.sigusr2} signal which should not be used
      for other purposes at the same time. *)

  type t
  (** Represents an optional interrupt request. *)

  val nothing : t
  (** A constant for a no request.  {{!clr} [clr nothing]} does nothing. *)

  val req : seconds:float -> t
  (** [req ~seconds] requests an interrupt in the form of a signal delivered to
      the thread that made the request within the specified number of [seconds].
      Blocking {!Unix} IO calls typically raise an error with the {{!Unix.EINTR}
      [Unix.EINTR]} error code when they are interrupted by a signal.

      Regardless of whether the signal gets triggered or a system call gets
      interrupted, the request must be {{!clr} cleared}.

      ⚠️ Due to limitations of the OCaml system modules and unlike with typical
      timeout mechanisms, the interrupt may also be triggered sooner. *)

  val clr : t -> unit
  (** [clr req] either cancels or acknowledges the interrupt request.  Every
      {!req} must be cleared! *)
end
