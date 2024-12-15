module type System = sig
  (** Signature for a module that {!Picos_lwt.run} requires for interoperating
      with the system that {!Lwt} runs on. *)

  val sleep : float -> unit Lwt.t
  (** [sleep seconds] returns a cancelable promise that resolves after the given
      number of seconds. *)

  type trigger
  (** Represents a semi thread-safe signaling mechanism. *)

  val trigger : unit -> trigger
  (** [trigger ()] returns a new thread-safe, single use signaling mechanism.

      ⚠️ This may only be called on the main thread on which {!Lwt} runs. *)

  val signal : trigger -> unit
  (** [signal trigger] resolves the promise that {{!await} [await trigger]}
      returns.

      ℹ️ It must be safe to call [signal] from any thread or domain. As a special
      case this need not be thread-safe in case the system only allows a single
      thread. *)

  val await : trigger -> unit Lwt.t
  (** [await trigger] returns a promise thet resolves, on the main thread, after
      {{!signal} [signal trigger]} has been called.

      ⚠️ This may only be called on the main thread on which {!Lwt} runs. *)
end
