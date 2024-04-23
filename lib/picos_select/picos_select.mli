(** Basic {!Unix.select} based IO event loop for {!Picos}.

    The operations in this module automatically manage a {!Thread} per domain
    that runs a {!Unix.select} loop to support the operations.

    ⚠️ Signal handlers are unfortunately fundamentally non-compositional.  The
    use of signal handlers in this module has been designed to be {{!configure}
    configurable}, which should allow co-operating with other libraries using
    signals as long as care is taken at application startup to {!configure}
    things.

    ⚠️ All the usual limitations of the {!Unix} module apply. *)

open Picos

(** {1 API} *)

(** {2 Timeouts} *)

val cancel_after : _ Computation.t -> seconds:float -> Exn_bt.t -> unit
(** [cancel_after computation ~seconds exn_bt] arranges for [computation] to be
    {{!Picos.Computation.cancel} canceled} with given [exn_bt] after given time
    in [seconds].  Completion of the [computation] before the specified time
    effectively cancels the timeout.

    ℹ️ You can use [cancel_after] to implement the handler for the
    {{!Picos.Computation.Cancel_after} [Cancel_after]} effect. *)

(** {2 IO} *)

val return_on : 'a Computation.t -> Picos_fd.t -> [ `R | `W | `E ] -> 'a -> unit
(** [return_on computation fd op value] arranges for [computation] to be
    {{!Picos.Computation.return} returned} with given [value] when [fd] becomes
    available for [op].  Completion of the [computation] before the [fd] becomes
    available for [op] effectively cancels the arrangement.

    ℹ️ Using {!Unix.set_nonblock} and [return_on] you can implement direct-style
    transparently asynchronous IO on top of the {!Unix} module. *)

val await_on : Picos_fd.t -> [ `R | `W | `E ] -> Picos_fd.t
(** [await_on fd op] awaits until [fd] becomes available for [op]. *)

module Intr : sig
  (** A mechanism to interrupt blocking {!Unix} IO operations.

      ⚠️ The mechanism uses {{!configure} a signal} which should not be used for
      other purposes.

      ⚠️ Beware that signal handling in OCaml 5.0.0 is known to be broken and
      several fixes were included in OCaml {{:https://ocaml.org/releases/5.1.0}
      5.1.0}. *)

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
      interrupted, the request must be {{!clr} cleared} exactly once!

      ⚠️ Due to limitations of the OCaml system modules and unlike with typical
      timeout mechanisms, the interrupt may also be triggered sooner. *)

  val clr : t -> unit
  (** [clr req] either cancels or acknowledges the interrupt request.  Every
      {!req} must be cleared exactly once! *)
end

(** {2 Processes} *)

val return_on_sigchld : 'a Computation.t -> 'a -> unit
(** [return_on_sigchld computation value] arranges for [computation] to be
    {{!Picos.Computation.return} returned} with given [value] on next
    {!Sys.sigchld}.  Completion of the [computation] before a {!Sys.sigchld} is
    received effectively cancels the arrangement.

    ⚠️ The mechanism uses the {!Sys.sigchld} signal which should not be used for
    other purposes. *)

(** {2 Configuration} *)

val configure : ?intr_sig:int -> ?handle_sigchld:bool -> unit -> unit
(** [configure ~intr_sig ~handle_sigchld ()] can, and sometimes must, be called
    by an application to configure the use of signals by this module.

    The optional [intr_sig] argument can be used to specify the signal used by
    the {{!Intr} interrupt} mechanism.  The default is to use {!Sys.sigusr2}.

    The optional [handle_sigchld] argument can be used to specify whether this
    module should setup handling of {!Sys.sigchld}.  The default is [true].
    When explicitly specified as [~handle_sigchld:false], the application should
    arrange to call {!handle_signal} whenever a {!Sys.sigchld} signal occurs.

    ⚠️ This module must always be configured before use.  Unless this module has
    been explicitly configured, calling a method of this module from the main
    thread on the main domain will automatically configure this module with
    default options.  In case the application uses multiple threads or multiple
    domains, the application should arrange to call [configure] from the main
    thread on the main domain before any threads or domains besides the main are
    created or spawned. *)

val handle_signal : int -> unit
(** [handle_signal signum] should be called to notify this module of a signal
    when {{!configure} configured} to not handle said signals. *)

val check_configured : unit -> unit
(** [check_configured ()] checks whether this module has already been
    {{!configure} configured} or not and, if not, calls {!configure} with
    default arguments.

    ℹ️ The intended use case for [check_configure ()] is at the point of
    entry of schedulers and other facilities that use this module. *)

(** {1 Examples}

    For convenience, we first open the {!Picos} and {!Picos_stdio} modules:

    {[
      open Foundation.Finally
      open Picos
      open Picos_stdio
    ]}

    {2 One of many}

    Here is an example that awaits for one of multiple alternative events:

    {[
      # exception Timeout
      exception Timeout

      # Picos_fifos.run ~forbid:false @@ fun () ->

        let@ msg_inn1, msg_out1 =
          finally Unix.close_pair @@ fun () ->
          Unix.socketpair PF_UNIX SOCK_STREAM 0 ~cloexec:true
        in
        let@ msg_inn2, msg_out2 =
          finally Unix.close_pair @@ fun () ->
          Unix.socketpair PF_UNIX SOCK_STREAM 0 ~cloexec:true
        in
        let@ syn_inn, syn_out =
          finally Unix.close_pair @@ fun () ->
          Unix.socketpair PF_UNIX SOCK_STREAM 0 ~cloexec:true
        in

        Unix.set_nonblock msg_inn1;
        Unix.set_nonblock msg_out1;
        Unix.set_nonblock msg_inn2;
        Unix.set_nonblock msg_out2;
        Unix.set_nonblock syn_inn;
        Unix.set_nonblock syn_out;

        let consumer = Computation.create () in
        Fiber.spawn ~forbid:false consumer [ fun () ->
          try
            while true do
              Fiber.check (Fiber.current ());

              let select = Computation.create () in
              Picos_select.return_on select msg_inn1 `R `Inn1;
              Picos_select.return_on select msg_inn2 `R `Inn2;
              Picos_select.cancel_after select
                ~seconds:0.1 (Exn_bt.get_callstack 0 Timeout);

              match Computation.await select with
              | `Inn1 ->
                Printf.printf "Inn1\n%!";
                assert (1 = Unix.read msg_inn1 (Bytes.create 1) 0 1);
                assert (1 = Unix.write_substring syn_out "!" 0 1)
              | `Inn2 ->
                Printf.printf "Inn2\n%!";
                assert (1 = Unix.read msg_inn2 (Bytes.create 1) 0 1);
                assert (1 = Unix.write_substring syn_out "!" 0 1)
              | exception Timeout ->
                Printf.printf "Timeout\n%!";
                assert (1 = Unix.write_substring syn_out "!" 0 1)
              | exception Exit ->
                Computation.cancel select (Exn_bt.get_callstack 0 Exit)
            done
          with Exit -> () ];

        assert (1 = Unix.write_substring msg_out1 "!" 0 1);
        assert (1 = Unix.read syn_inn (Bytes.create 1) 0 1);
        assert (1 = Unix.write_substring msg_out2 "!" 0 1);
        assert (1 = Unix.read syn_inn (Bytes.create 1) 0 1);
        assert (1 = Unix.read syn_inn (Bytes.create 1) 0 1);

        Computation.cancel consumer (Exn_bt.get_callstack 0 Exit)
      Inn1
      Inn2
      Timeout
      - : unit = ()
    ]}

    This approach of using the completion of a computation to select one of
    multiple events can be generalized. *)
