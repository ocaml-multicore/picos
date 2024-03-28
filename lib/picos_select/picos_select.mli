(** A basic {!Unix.select} based IO event loop for {!Picos}.

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

val return_on :
  'a Computation.t ->
  Unix.file_descr Picos_rc.t ->
  [ `R | `W | `E ] ->
  'a ->
  unit
(** [return_on computation file_descr op value] arranges for [computation] to be
    {{!Picos.Computation.return} returned} with given [value] when [file_descr]
    becomes available for [op].  Completion of the [computation] before the
    [file_descr] becomes available for [op] effectively cancels the await.

    ℹ️ Using {!Unix.set_nonblock} and [return_on] you can implement direct-style
    transparently asynchronous IO on top of the [Unix] module. *)

val await_on : Unix.file_descr Picos_rc.t -> [ `R | `W | `E ] -> unit
(** [await_on file_descr op] awaits until [file_descr] becomes available for
    [op]. *)
