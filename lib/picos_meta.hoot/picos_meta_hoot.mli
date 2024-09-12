(** The beginnings of an actor model implementation loosely inspired by
    {{:https://github.com/riot-ml/riot} Riot}.

    ⚠️ This is meant as an example only and would require further development to
    become a production ready actor model implementation. *)

(** {1 Modules} *)

module Hoot : sig
  (** {{:https://www.merriam-webster.com/thesaurus/riot} Hoot} is an actor model
      core loosely inspired by {{:https://github.com/riot-ml/riot} Riot}. *)

  exception Terminate
  (** Exception used by {!link} to terminate actor processes.

      An unhandled [Terminate] exception is not treated as an error. *)

  val run : (unit -> unit) -> unit
  (** [run main] establishes a new actor process on the current fiber and runs
      [main] as the process.

      ⚠️ An unhandled exception, aside from {!Terminate}, raised from [main ()]
      will be raised from [run main]. Unlike with {!spawn} an unhandled
      exception is not treated as a fatal error.

      This can be called from any fiber, even from another actor process, which
      would then effectively get suspended while running [main], but typically
      this is used to start a "scope" for running actors. *)

  module Pid : sig
    (** Actor process or process identifier. *)

    type t
  end

  val spawn : (unit -> unit) -> Pid.t
  (** [spawn main] creates a new actor process to run [main].

      ⚠️ An unhandled exception, aside from {!Terminate}, raised from [main ()]
      will be treated as a fatal error and will either exit the entire program
      or stop the scheduler without completing other fibers.

      This can be called from any fiber, even from fibers that are not actor
      processes, but typically this would be used within a "scope" for running
      actors. *)

  val self : unit -> Pid.t
  (** [self ()], when called within an actor process, returns the
      {{!Pid.t} process identifier} of the actor process.

      @raise Invalid_argument when called outside of an actor process. *)

  val wait : Pid.t -> unit
  (** [wait pid] blocks until the specified actor process has terminated. *)

  module Message : sig
    (** Extensible message type. *)

    type t = ..
  end

  val receive : unit -> Message.t
  (** [receive ()] waits until at least one message has been added to the
      mailbox of the current process and then removes and returns the least
      recently added message from the mailbox. *)

  val send : Pid.t -> Message.t -> unit
  (** [send pid message] adds the given [message] to the mailbox of the process
      [pid].

      ℹ️ Sending a message to a process that has already terminated is not
      considered an error. *)

  type Message.t +=
    | Terminated of Pid.t
          (** Sent by the {!monitor} mechanism to notify of process termination.
          *)

  val monitor : at:Pid.t -> the:Pid.t -> unit
  (** [monitor ~at:observer ~the:subject] makes it so that when the [subject]
      process terminates the message {{!Terminated} [Terminated subject]} is
      {{!send} sent} to the [observer] process. *)

  val link : Pid.t -> Pid.t -> unit
  (** [link pid1 pid2] makes it so that when either one of the given processes
      terminates the other process will also be terminated with the [Terminate]
      exception.

      In case either one of the given processes is already terminated when
      [link] is called, the other process will then be terminated. *)
end

(** {1 Examples} *)
