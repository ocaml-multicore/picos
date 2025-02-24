(** Basic {!Unix.select} based IO event loop for {!Picos}.

    The operations in this module automatically manage a {!Thread} per domain
    that runs a {!Unix.select} loop to support the operations.

    ⚠️ Signal handlers are unfortunately fundamentally non-compositional. The use
    of signal handlers in this module has been designed to be
    {{!configure} configurable}, which should allow co-operating with other
    libraries using signals as long as care is taken at application startup to
    {!configure} things.

    ⚠️ All the usual limitations of the {!Unix} module apply. *)

open Picos
open Picos_std_event

(** {1 API} *)

(** {2 Timeouts} *)

val cancel_after :
  _ Computation.t -> seconds:float -> exn -> Printexc.raw_backtrace -> unit
(** [cancel_after computation ~seconds exn bt] arranges for [computation] to be
    {{!Picos.Computation.cancel} canceled} with given exception and backtrace
    after given time in [seconds]. Completion of the [computation] before the
    specified time effectively cancels the timeout.

    ℹ️ You can use [cancel_after] to implement the handler for the
    {{!Picos.Computation.Cancel_after} [Cancel_after]} effect. *)

val timeout : seconds:float -> unit Event.t
(** [timeout ~seconds] returns an {{!Picos_std_event.Event} event} that, on each
    time after being {{!Picos_std_event.Event.sync} synchronized} on, can be
    committed to after the specified number of [seconds]. *)

(** {2 IO} *)

val return_on :
  'a Computation.t -> Picos_io_fd.t -> [ `R | `W | `E ] -> 'a -> unit
(** [return_on computation fd op value] arranges for [computation] to be
    {{!Picos.Computation.return} returned} with given [value] when [fd] becomes
    available for [op]. Completion of the [computation] before the [fd] becomes
    available for [op] effectively cancels the arrangement.

    ℹ️ Using {!Unix.set_nonblock} and [return_on] you can implement direct-style
    transparently asynchronous IO on top of the {!Unix} module. *)

val await_on : Picos_io_fd.t -> [ `R | `W | `E ] -> Picos_io_fd.t
(** [await_on fd op] awaits until [fd] becomes available for [op]. *)

val on : Picos_io_fd.t -> [ `R | `W | `E ] -> unit Event.t
(** [on fd op] returns an {{!Picos_std_event.Event} event} that can be committed
    to when [fd] becomes available for [op]. *)

module Intr : sig
  (** A mechanism to interrupt blocking {!Unix} IO operations.

      ⚠️ The mechanism uses {{!configure} a signal} which should not be used for
      other purposes.

      ⚠️ Beware that signal handling in OCaml 5.0.0 is known to be broken and
      several fixes were included in OCaml
      {{:https://ocaml.org/releases/5.1.0} 5.1.0}. *)

  type t
  (** Represents an optional interrupt request. *)

  val nothing : t
  (** A constant for a no request. {{!clr} [clr nothing]} does nothing. *)

  val req : seconds:float -> t
  (** [req ~seconds] requests an interrupt in the form of a signal delivered to
      the thread that made the request within the specified number of [seconds].
      Blocking {!Unix} IO calls typically raise an error with the
      {{!Unix.EINTR} [Unix.EINTR]} error code when they are interrupted by a
      signal. Regardless of whether the signal gets triggered or a system call
      gets interrupted, the request must be {{!clr} cleared} exactly once!

      ⚠️ Due to limitations of the OCaml system modules and unlike with typical
      timeout mechanisms, the interrupt may also be triggered sooner. *)

  val clr : t -> unit
  (** [clr req] either cancels or acknowledges the interrupt request. Every
      {!req} must be cleared exactly once! *)
end

(** {2 Processes} *)

val return_on_sigchld : 'a Computation.t -> 'a -> unit
(** [return_on_sigchld computation value] arranges for [computation] to be
    {{!Picos.Computation.return} returned} with given [value] on next
    {!Sys.sigchld}. Completion of the [computation] before a {!Sys.sigchld} is
    received effectively cancels the arrangement.

    ⚠️ The mechanism uses the {!Sys.sigchld} signal which should not be used for
    other purposes. *)

val on_sigchld : unit Event.t
(** [on_sigchld] is an {{!Picos_std_event.Event} event} that can be committed to
    after a {!Sys.sigchld} signal has occurred. *)

(** {2 Configuration} *)

val configure :
  ?intr_sig:int -> ?handle_sigchld:bool -> ?ignore_sigpipe:bool -> unit -> unit
(** [configure ~intr_sig ~handle_sigchld ()] can, and sometimes must, be called
    by an application to configure the use of signals by this module.

    The optional [intr_sig] argument can be used to specify the signal used by
    the {{!Intr} interrupt} mechanism. The default is to use {!Sys.sigusr2}.

    The optional [handle_sigchld] argument can be used to specify whether this
    module should setup handling of {!Sys.sigchld}. The default is [true]. When
    explicitly specified as [~handle_sigchld:false], the application should
    arrange to call {!handle_signal} whenever a {!Sys.sigchld} signal occurs.

    The optional [ignore_sigpipe] argument can be used to specify whether
    {!Sys.sigpipe} will be configured to be ignored or not. The default is
    [true].

    ⚠️ This module must always be configured before use. Unless this module has
    been explicitly configured, calling a method of this module from the main
    thread on the main domain will automatically configure this module with
    default options. In case the application uses multiple threads or multiple
    domains, the application should arrange to call [configure] from the main
    thread on the main domain before any threads or domains besides the main are
    created or spawned. *)

val check_configured : unit -> unit
(** [check_configured ()] checks whether this module has already been
    {{!configure} configured} or not and, if not, calls {!configure} with
    default arguments. In either case, calling [check_configured ()] will
    (re)configure signal handling for the current thread and perform other
    required initialization for the thread to use this module.

    ⚠️ This should be called at the start of every thread using this module.

    ℹ️ The intended use case for [check_configured ()] is at the point of entry
    of schedulers and other facilities that use this module. In other words,
    application code should ideally not need to call this directly. *)

val handle_signal : int -> unit
(** [handle_signal signum] should be called to notify this module of a signal
    when {{!configure} configured} to not handle said signals. *)

(** {1 Examples}

    First we open some modules for convenience:

    {[
      open Picos
      open Picos_io
      open Picos_std_event
      open Picos_std_finally
      open Picos_std_structured
    ]}

    {2 One of many}

    Here is an example that awaits for one of multiple alternative events:

    {[
      # Picos_mux_random.run_on ~n_domains:2 @@ fun () ->

        let@ msg_inn1, msg_out1 =
          finally Unix.close_pair @@ fun () ->
          Unix.socketpair ~cloexec:true
            PF_UNIX SOCK_STREAM 0
        in
        let@ msg_inn2, msg_out2 =
          finally Unix.close_pair @@ fun () ->
          Unix.socketpair ~cloexec:true
            PF_UNIX SOCK_STREAM 0
        in
        let@ syn_inn, syn_out =
          finally Unix.close_pair @@ fun () ->
          Unix.socketpair ~cloexec:true
            PF_UNIX SOCK_STREAM 0
        in

        Unix.set_nonblock msg_inn1;
        Unix.set_nonblock msg_out1;
        Unix.set_nonblock msg_inn2;
        Unix.set_nonblock msg_out2;
        Unix.set_nonblock syn_inn;
        Unix.set_nonblock syn_out;

        let read1 fd =
          let r =
            Unix.read fd (Bytes.create 1) 0 1
          in
          assert (r = 1)
        and write1 fd =
          let w =
            Unix.write_substring fd "!" 0 1
          in
          assert (w = 1)
        in

        Flock.join_after ~on_return:`Terminate begin fun () ->
          Flock.fork begin fun () ->
            while true do
              Event.select [
                Picos_io_select.on msg_inn1 `R
                  |> Event.map begin fun () ->
                    print_endline "Inn1";
                    read1 msg_inn1;
                    write1 syn_out
                  end;
                Picos_io_select.on msg_inn2 `R
                  |> Event.map begin fun () ->
                    print_endline "Inn2";
                    read1 msg_inn2;
                    write1 syn_out;
                  end;
                Picos_io_select.timeout
                    ~seconds:60.0
                  |> Event.map begin fun () ->
                    print_endline "Timeout";
                    write1 syn_out
                  end;
              ]
            done
          end;

          write1 msg_out1;
          read1 syn_inn;
          write1 msg_out2;
          read1 syn_inn;
        end
      Inn1
      Inn2
      - : unit = ()
    ]}

    Above we use the {{!Picos_std_event.Event} [Event]} module providing
    composable events. *)
