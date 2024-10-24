(** Basic {{:https://en.wikipedia.org/wiki/Futex} futex}-like awaitable atomic
    location for {!Picos}. *)

(** {1 Modules} *)

module Awaitable : sig
  (** An awaitable atomic location.

      This module provides a superset of the Stdlib {!Atomic} API with more or
      less identical performance.  The main difference is that a non-padded
      awaitable location takes an extra word of memory.  Additionally a
      {{:https://en.wikipedia.org/wiki/Futex} futex}-like API provides the
      ability to {!await} until an awaitable location is explicitly {!signal}ed
      to potentially have a different value.

      Awaitable locations can be used to implement many kinds of synchronization
      and communication primitives. *)

  (** {1 Atomic API} *)

  type 'a t
  (** Represents an awaitable atomic location. *)

  val make : ?padded:bool -> 'a -> 'a t
  (** [make initial] creates a new awaitable atomic location with the given
      [initial] value. *)

  val make_contended : 'a -> 'a t
  (** [make_contended initial] is equivalent to {{!make} [make ~padded:true initial]}. *)

  val get : 'a t -> 'a
  (** [get awaitable] is essentially equivalent to [Atomic.get awaitable]. *)

  val compare_and_set : 'a t -> 'a -> 'a -> bool
  (** [compare_and_set awaitable before after] is essentially equivalent to
      [Atomic.compare_and_set awaitable before after]. *)

  val exchange : 'a t -> 'a -> 'a
  (** [exchange awaitable after] is essentially equivalent to [Atomic.exchange awaitable after]. *)

  val set : 'a t -> 'a -> unit
  (** [set awaitable value] is equivalent to {{!exchange} [exchange awaitable value |> ignore]}. *)

  val fetch_and_add : int t -> int -> int
  (** [fetch_and_add awaitable delta] is essentially equivalent to
      [Atomic.fetch_and_add awaitable delta]. *)

  val incr : int t -> unit
  (** [incr awaitable] is equivalent to {{!fetch_and_add} [fetch_and_add awaitable (+1) |> ignore]}. *)

  val decr : int t -> unit
  (** [incr awaitable] is equivalent to {{!fetch_and_add} [fetch_and_add awaitable (-1) |> ignore]}. *)

  (** {1 Futex API} *)

  val signal : 'a t -> unit
  (** [signal awaitable] tries to wake up one fiber {!await}in on the awaitable
      location.

      🐌 Generally speaking one should avoid calling [signal] too frequently,
      because the queue of awaiters is stored separately from the awaitable
      location and it takes a bit of effort to locate it.  For example, calling
      [signal] every time a value is added to an empty data structure might not
      be optimal.  In many cases it is faster to explicitly mark the potential
      presence of awaiters in the data structure and avoid calling [signal] when
      it is definitely known that there are no awaiters. *)

  val broadcast : 'a t -> unit
  (** [broadcast awaitable] tries to wake up all fibers {!await}ing on the
      awaitable location.

      🐌 The same advice as with {!signal} applies to [broadcast].  In addition,
      it is typically a good idea to avoid potentially waking up large numbers
      of fibers as it can easily lead to the
      {{:https://en.wikipedia.org/wiki/Thundering_herd_problem} thundering herd}
      phenomana. *)

  val await : 'a t -> 'a -> unit
  (** [await awaitable before] suspends the current fiber until the awaitable is
      explicitly {!signal}ed and has a value other than [before].

      ⚠️ This operation is subject to the
      {{:https://en.wikipedia.org/wiki/ABA_problem} ABA} problems.  An [await]
      for value other than [A] may not return after the awaitable is signaled
      while having the value [B], because at a later point the awaitable has
      again the value [A].  Furthermore, by the time an [await] for value other
      than [A] returns, the awaitable might already again have the value [A].

      ⚠️ Atomic operations that change the value of an awaitable do not
      implicitly wake up awaiters. *)

  module Awaiter : sig
    (** Ability to await for a signal from the past.

        {!Awaitable.await} only receives a signal at or after the point of
        calling it.  This API allows the awaiting process to be broken into two
        steps, {!add} and {!await}, such that a signal after {!add} can be
        received by {!await}. *)

    type 'a awaitable := 'a t
    (** An erased type alias for {!Awaitable.t}. *)

    type t
    (** Represents a single use awaiter of a signal to an {!awaitable}. *)

    val add : 'a awaitable -> t
    (** [add awaitable] create a single use awaiter, adds it to the FIFO
        associated with the awaitable, and returns the awaiter. *)

    val await : t -> unit
    (** [await awaiter] awaits for the association awaitable to be signaled. *)

    val remove : t -> unit
    (** [remove awaiter] marks the awaiter as having been signaled and removes it
        from the FIFO associated with the awaitable.

        ⚠️ An explicit call of [remove] is needed when an {!add}ed awaiter is not
        {!await}ed for.  In such a case, from the point of view of lost signals,
        the caller of [remove] should be considered to have received or consumed
        a signal before the call of [remove]. *)
  end
end

(** {1 Examples}

    We first open the library to bring the {!Awaitable} module into scope:

    {[
      # open Picos_std_awaitable
    ]}

    {2 [Mutex]}

    Here is a basic mutex implementation using awaitables:

    {[
      module Mutex = struct
        type t = int Awaitable.t

        let create ?padded () = Awaitable.make ?padded 0

        let lock t =
          if not (Awaitable.compare_and_set t 0 1) then
            while Awaitable.exchange t 2 <> 0 do
              Awaitable.await t 2
            done

        let unlock t =
          let before = Awaitable.fetch_and_add t (-1) in
          if before = 2 then begin
            Awaitable.set t 0;
            Awaitable.signal t
          end
      end
    ]}

    The above mutex outperforms most other mutexes under both no/low and high
    contention scenarios.  In no/low contention scenarios the use of
    {{!Awaitable.fetch_and_add} [fetch_and_add]} provides low overhead.  In high
    contention scenarios the above mutex allows unfairness, which avoids
    performance degradation due to the
    {{:https://en.wikipedia.org/wiki/Lock_convoy} lock convoy} phenomena.

    {2 [Condition]}

    Let's also implement a condition variable.  For that we'll also make use of
    low level operations in the {!Picos} core library:

    {[
      # open Picos
    ]}

    To implement a condition variable, we'll use the {{!Awaitable.Awaiter}
    [Awaiter]} API:

    {[
      module Condition = struct
        type t = unit Awaitable.t

        let create () = Awaitable.make ()

        let wait t mutex =
          let awaiter = Awaitable.Awaiter.add t in
          Mutex.unlock mutex;
          let lock_forbidden mutex =
            let fiber = Fiber.current () in
            let forbid = Fiber.exchange fiber ~forbid:true in
            Mutex.lock mutex;
            Fiber.set fiber ~forbid
          in
          match Awaitable.Awaiter.await awaiter with
          | () -> lock_forbidden mutex
          | exception exn ->
              let bt = Printexc.get_raw_backtrace () in
              lock_forbidden mutex;
              Printexc.raise_with_backtrace exn bt

        let signal = Awaitable.signal
        let broadcast = Awaitable.broadcast
      end
    ]}

    Notice that the awaitable location used in the above condition variable
    implementation is never mutated.  We just reuse the signaling mechanism of
    awaitables. *)
