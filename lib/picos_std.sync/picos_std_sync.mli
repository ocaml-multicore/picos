(** Basic communication and synchronization primitives for {!Picos}.

    This library essentially provides a conventional set of communication and
    synchronization primitives for concurrent programming with any Picos
    compatible scheduler.

    For the {{!examples} examples} we open some modules:

    {[
      open Picos_std_structured
      open Picos_std_sync
    ]} *)

open Picos_std_event

(** {1 Modules} *)

module Mutex : sig
  (** A mutual-exclusion lock or mutex.

      ℹ️ This intentionally mimics the interface of {!Stdlib.Mutex}. Unlike with
      the standard library mutex, blocking on this mutex potentially allows an
      effects based scheduler to run other fibers on the thread.

      🏎️ The optional [checked] argument taken by most of the operations defaults
      to [true]. When explicitly specified as [~checked:false] the mutex
      implementation may avoid having to obtain the
      {{!Picos.Fiber.current} current fiber}, which can be expensive relative to
      locking or unlocking an uncontested mutex. Note that specifying
      [~checked:false] on an operation may prevent error checking also on a
      subsequent operation.

      See also {!Lock}. *)

  type t
  (** Represents a mutual-exclusion lock or mutex. *)

  val create : ?padded:bool -> unit -> t
  (** [create ()] returns a new mutex that is initially unlocked. *)

  val lock : ?checked:bool -> t -> unit
  (** [lock mutex] locks the [mutex].

      ℹ️ If the fiber has been canceled and propagation of cancelation is
      allowed, this may raise the cancelation exception before locking the
      mutex. If [~checked:false] was specified, the cancelation exception may or
      may not be raised.

      @raise Sys_error
        if the mutex is already locked by the fiber. If [~checked:false] was
        specified for some previous operation on the mutex the exception may or
        may not be raised. *)

  val try_lock : ?checked:bool -> t -> bool
  (** [try_lock mutex] locks the mutex in case the mutex is unlocked. Returns
      [true] on success and [false] in case the mutex was locked.

      ℹ️ If the fiber has been canceled and propagation of cancelation is
      allowed, this may raise the cancelation exception before locking the
      mutex. If [~checked:false] was specified, the cancelation exception may or
      may not be raised. *)

  val unlock : ?checked:bool -> t -> unit
  (** [unlock mutex] unlocks the mutex.

      ℹ️ This operation is not cancelable.

      @raise Sys_error
        if the mutex was locked by another fiber. If [~checked:false] was
        specified for some previous operation on the mutex the exception may or
        may not be raised. *)

  val protect : ?checked:bool -> t -> (unit -> 'a) -> 'a
  (** [protect mutex thunk] locks the [mutex], runs [thunk ()], and unlocks the
      [mutex] after [thunk ()] returns or raises.

      ℹ️ If the fiber has been canceled and propagation of cancelation is
      allowed, this may raise the cancelation exception before locking the
      mutex. If [~checked:false] was specified, the cancelation exception may or
      may not be raised.

      @raise Sys_error for the same reasons as {!lock} and {!unlock}. *)
end

module Condition : Intf.Condition with type lock := Mutex.t
(** A condition variable.

    ℹ️ This intentionally mimics the interface of {!Stdlib.Condition}. Unlike
    with the standard library condition variable, blocking on this condition
    variable allows an effects based scheduler to run other fibers on the
    thread. *)

module Semaphore : sig
  (** {!Counting} and {!Binary} semaphores.

      ℹ️ This intentionally mimics the interface of {!Stdlib.Semaphore}. Unlike
      with the standard library semaphores, blocking on these semaphores allows
      an effects based scheduler to run other fibers on the thread.

      See also {!Sem}. *)

  module Counting : sig
    (** A counting semaphore. *)

    type t
    (** Represents a counting semaphore. *)

    val make : ?padded:bool -> int -> t
    (** [make initial] creates a new counting semaphore with the given [initial]
        count.

        @raise Invalid_argument in case the given [initial] count is negative.
    *)

    val release : t -> unit
    (** [release semaphore] increments the count of the semaphore.

        ℹ️ This operation is not cancelable.

        @raise Sys_error in case the count would overflow. *)

    val acquire : t -> unit
    (** [acquire semaphore] waits until the count of the semaphore is greater
        than [0] and then atomically decrements the count. *)

    val try_acquire : t -> bool
    (** [try_acquire semaphore] attempts to atomically decrement the count of
        the semaphore unless the count is already [0]. *)

    val get_value : t -> int
    (** [get_value semaphore] returns the current count of the semaphore. This
        should only be used for debugging or informational messages. *)
  end

  module Binary : sig
    (** A binary semaphore. *)

    type t
    (** Represents a binary semaphore. *)

    val make : ?padded:bool -> bool -> t
    (** [make initial] creates a new binary semaphore with count of [1] in case
        [initial] is [true] and count of [0] otherwise. *)

    val release : t -> unit
    (** [release semaphore] sets the count of the semaphore to [1].

        ℹ️ This operation is not cancelable. *)

    val acquire : t -> unit
    (** [acquire semaphore] waits until the count of the semaphore is [1] and
        then atomically changes the count to [0]. *)

    val try_acquire : t -> bool
    (** [try_acquire semaphore] attempts to atomically change the count of the
        semaphore from [1] to [0]. *)
  end
end

module Lock : sig
  (** A mutual exclusion lock.

      🏎️ This uses a low overhead, optimistic, and unfair implementation that
      also does not perform runtime ownership error checking. In most cases this
      should be the mutual exclusion lock you will want to use.

      See also {!Mutex}. *)

  type t
  (** Represents a mutual exclusion lock. *)

  (** {1 Basic API} *)

  val create : ?padded:bool -> unit -> t
  (** [create ()] returns a new mutual exclusion lock that is initially
      unlocked. *)

  exception Poisoned
  (** Exception raised in case the lock has been {{!poison} poisoned}. *)

  val holding : t -> (unit -> 'a) -> 'a
  (** [holding lock thunk] acquires the [lock] and calls [thunk ()]. In case
      [thunk ()] returns a value, the lock is released and the value is
      returned. Otherwise the lock is poisoned and the exception is reraised.

      The implementation of {!holding} in terms of the low level operations is
      equivalent to:

      {[
        let holding t thunk =
          Lock.acquire t;
          match thunk () with
          | value ->
              Lock.release t;
              value
          | exception exn ->
              let bt = Printexc.get_raw_backtrace () in
              Lock.poison t;
              Printexc.raise_with_backtrace exn bt
      ]}

      @raise Poisoned in case the lock has been {{!poison} poisoned}. *)

  val protect : t -> (unit -> 'a) -> 'a
  (** [protect lock thunk] acquires the [lock], runs [thunk ()], and releases
      the [lock] after [thunk ()] returns or raises.

      @raise Poisoned in case the lock has been {{!poison} poisoned}. *)

  module Condition : Intf.Condition with type lock = t
  (** A condition variable. *)

  (** {1 State query API} *)

  val is_locked : t -> bool
  (** [is_locked lock] determines whether the [lock] is currently held
      exclusively. *)

  val is_poisoned : t -> bool
  (** [is_poisoned lock] determines whether the [lock] has been
      {{!poison} poisoned}. *)

  (** {1 Expert API}

      ⚠️ The calls in this section must be matched correctly or the state of the
      lock may become corrupted. *)

  val acquire : t -> unit
  (** [acquire lock] acquires the [lock] exlusively.

      @raise Poisoned in case the [lock] has been {{!poison} poisoned}. *)

  val try_acquire : t -> bool
  (** [try_acquire lock] attempts to acquire the [lock] exclusively. Returns
      [true] in case of success and [false] in case of failure.

      @raise Poisoned in case the [lock] has been {{!poison} poisoned}. *)

  val release : t -> unit
  (** [release lock] releases the [lock] or does nothing in case the lock has
      been {{!poison} poisoned}. *)

  val poison : t -> unit
  (** [poison lock] marks an exclusively held [lock] as poisoned.

      @raise Invalid_argument
        in case the [lock] is not currently held exclusively. *)
end

module Sem : sig
  (** A counting semaphore.

      🏎️ This uses a low overhead, optimistic, and unfair implementation. In most
      cases this should be the semaphore you will want to use.

      See also {!Semaphore}. *)

  type t
  (** Represents a counting semaphore. *)

  val max_value : int
  (** Maximum counter value allowed by the semaphore implementation.

      ℹ️ The exact maximum value is unspecified, but should typically be no less
      than {!Sys.max_array_length}. *)

  val create : ?padded:bool -> int -> t
  (** [create initial] creates a new counting semaphore with the given [initial]
      count.

      @raise Invalid_argument
        in case the given [initial] count is negative or higher than
        {!max_value}. *)

  val release : t -> unit
  (** [release sem] increments the count of the semaphore or does nothing in
      case the semaphore has been {{!poison} poisoned}.

      ℹ️ This operation is not cancelable.

      @raise Sys_error in case the count would overflow. *)

  exception Poisoned
  (** Exception raised in case the semaphore has been {{!poison} poisoned}. *)

  val acquire : t -> unit
  (** [acquire sem] waits until the count of the semaphore is greater than [0]
      and then atomically decrements the count.

      @raise Poisoned in case the semaphore has been {{!poison} poisoned}. *)

  val try_acquire : t -> bool
  (** [try_acquire sem] attempts to atomically decrement the count of the
      semaphore unless the count is already [0].

      @raise Poisoned in case the semaphore has been {{!poison} poisoned}. *)

  val get_value : t -> int
  (** [get_value sem] returns the current count of the semaphore or [0] in case
      the semaphore has been {{!poison} poisoned}.

      ℹ️ This should only be used for debugging or informational messages. *)

  val poison : t -> unit
  (** [poison sem] marks the semaphore as poisoned. *)

  val is_poisoned : t -> bool
  (** [is_poisoned sem] determines whether the semaphore has been
      {{!poison} poisoned}. *)
end

module Lazy : sig
  (** A lazy suspension.

      ℹ️ This intentionally mimics the interface of {!Stdlib.Lazy}. Unlike with
      the standard library suspensions an attempt to force a suspension from
      multiple fibers, possibly running on different domains, does not raise the
      {!Undefined} exception. *)

  exception Undefined
  (** Synonym for {!Stdlib.Lazy.Undefined}. *)

  type !'a t
  (** Represents a deferred computation or suspension. *)

  val from_fun : (unit -> 'a) -> 'a t
  (** [from_fun thunk] returns a suspension. *)

  val from_val : 'a -> 'a t
  (** [from_val value] returns an already forced suspension whose result is the
      given [value]. *)

  val is_val : 'a t -> bool
  (** [is_val susp] determines whether the suspension has already been forced
      and didn't raise an exception. *)

  val force : 'a t -> 'a
  (** [force susp] forces the suspension, i.e. computes [thunk ()] using the
      [thunk] passed to {!from_fun}, stores the result of the computation to the
      suspension and reproduces its result. In case the suspension has already
      been forced the computation is skipped and stored result is reproduced.

      ℹ️ This will check whether the current fiber has been canceled before
      starting the computation of [thunk ()]. This allows the suspension to be
      forced by another fiber. However, if the fiber is canceled and the
      cancelation exception is raised after the computation has been started,
      the suspension will then store the cancelation exception.

      @raise Undefined
        in case the suspension is currently being forced by the
        {{!Picos.Fiber.current} current} fiber. *)

  val force_val : 'a t -> 'a
  (** [force_val] is a synonym for {!force}. *)

  val map : ('a -> 'b) -> 'a t -> 'b t
  (** [map fn susp] is equivalent to
      {{!from_fun} [from_fun (fun () -> fn (force susp))]}. *)

  val map_val : ('a -> 'b) -> 'a t -> 'b t
  (** [map_val fn susp] is equivalent to:
      {@ocaml skip[
        if is_val susp then from_val (fn (force susp)) else map fn susp
      ]} *)
end

module Latch : sig
  (** A dynamic single-use countdown latch.

      Latches are typically used for determining when a finite set of parallel
      computations is done. If the size of the set is known a priori, then the
      latch can be initialized with the size as initial count and then each
      computation just {{!decr} decrements} the latch.

      If the size is unknown, i.e. it is determined dynamically, then a latch is
      initialized with a count of one, the a priori known computations are
      started and then the latch is {{!decr} decremented}. When a computation is
      stsrted, the latch is {{!try_incr} incremented}, and then
      {{!decr} decremented} once the computation has finished. *)

  type t
  (** Represents a dynamic countdown latch. *)

  val create : ?padded:bool -> int -> t
  (** [create initial] creates a new countdown latch with the specified
      [initial] count.

      @raise Invalid_argument in case the specified [initial] count is negative.
  *)

  val try_decr : t -> bool
  (** [try_decr latch] attempts to decrement the count of the latch and returns
      [true] in case the count of the latch was greater than zero and [false] in
      case the count already was zero. *)

  val decr : t -> unit
  (** [decr latch] is equivalent to:
      {@ocaml skip[
        if not (try_decr latch) then invalid_arg "zero count"
      ]}

      ℹ️ This operation is not cancelable.

      @raise Invalid_argument in case the count of the latch is zero. *)

  val try_incr : t -> bool
  (** [try_incr latch] attempts to increment the count of the latch and returns
      [true] on success and [false] on failure, which means that the latch has
      already reached zero. *)

  val incr : t -> unit
  (** [incr latch] is equivalent to:
      {@ocaml skip[
        if not (try_incr latch) then invalid_arg "zero count"
      ]}

      @raise Invalid_argument in case the count of the latch is zero. *)

  val await : t -> unit
  (** [await latch] returns after the count of the latch has reached zero. *)

  val await_evt : t -> unit Event.t
  (** [await_evt latch] returns an event that can be committed to once the count
      of the latch has reached zero. *)
end

module Ivar : sig
  (** An incremental or single-assignment poisonable variable. *)

  type !'a t
  (** Represents an incremental variable. *)

  val create : unit -> 'a t
  (** [create ()] returns a new empty incremental variable. *)

  val of_value : 'a -> 'a t
  (** [of_value value] returns an incremental variable prefilled with the given
      [value]. *)

  val try_fill : 'a t -> 'a -> bool
  (** [try_fill ivar value] attempts to assign the given [value] to the
      incremental variable. Returns [true] on success and [false] in case the
      variable had already been poisoned or assigned a value. *)

  val fill : 'a t -> 'a -> unit
  (** [fill ivar value] is equivalent to
      {{!try_fill} [try_fill ivar value |> ignore]}. *)

  val try_poison_at : 'a t -> exn -> Printexc.raw_backtrace -> bool
  (** [try_poison_at ivar exn bt] attempts to poison the incremental variable
      with the specified exception and backtrace. Returns [true] on success and
      [false] in case the variable had already been poisoned or assigned a
      value.

      ℹ️ This operation is not cancelable. *)

  val try_poison : ?callstack:int -> 'a t -> exn -> bool
  (** [try_poison ivar exn] is equivalent to
      {{!try_poison_at} [try_poison_at ivar exn (Printexc.get_callstack n)]}
      where [n] defaults to [0]. *)

  val poison_at : 'a t -> exn -> Printexc.raw_backtrace -> unit
  (** [poison_at ivar exn bt] is equivalent to
      {{!try_poison_at} [try_poison_at ivar exn bt |> ignore]}. *)

  val poison : ?callstack:int -> 'a t -> exn -> unit
  (** [poison ivar exn] is equivalent to
      {{!poison_at} [poison_at ivar exn (Printexc.get_callstack n)]} where [n]
      defaults to [0]. *)

  val peek_opt : 'a t -> 'a option
  (** [peek_opt ivar] either returns [Some value] in case the variable has been
      assigned the [value], raises an exception in case the variable has been
      poisoned, or otherwise returns [None], which means that the variable has
      not yet been poisoned or assigned a value. *)

  val read : 'a t -> 'a
  (** [read ivar] waits until the variable is either assigned a value or the
      variable is poisoned and then returns the value or raises the exception.
  *)

  val read_evt : 'a t -> 'a Event.t
  (** [read_evt ivar] returns an event that can be committed to once the
      variable has either been assigned a value or has been poisoned. *)
end

module Stream : sig
  (** A lock-free, poisonable, many-to-many, stream.

      Readers can {!tap} into a stream to get a {!cursor} for reading all the
      values {{!push} pushed} to the stream starting from the {!cursor}
      position. Conversely, values {{!push} pushed} to a stream are lost unless
      a reader has a {!cursor} to the position in the stream. *)

  type !'a t
  (** Represents a stream of values of type ['a]. *)

  val create : ?padded:bool -> unit -> 'a t
  (** [create ()] returns a new stream. *)

  val push : 'a t -> 'a -> unit
  (** [push stream value] adds the [value] to the current position of the
      [stream] and advances the stream to the next position unless the [stream]
      has been {{!poison} poisoned} in which case only the exception given to
      {!poison} will be raised. *)

  val poison_at : 'a t -> exn -> Printexc.raw_backtrace -> unit
  (** [poison_at stream exn bt] marks the stream as poisoned at the current
      position, which means that subsequent attempts to {!push} to the [stream]
      will raise the given exception with backtrace.

      ℹ️ This operation is not cancelable. *)

  val poison : ?callstack:int -> 'a t -> exn -> unit
  (** [poison stream exn] is equivalent to
      {{!poison_at} [poison_at stream exn (Printexc.get_callstack n)]} where [n]
      defaults to [0]. *)

  type !'a cursor
  (** Represents a (past or current) position in a stream. *)

  val tap : 'a t -> 'a cursor
  (** [tap stream] returns a {!cursor} to the current position of the [stream].
  *)

  val peek_opt : 'a cursor -> ('a * 'a cursor) option
  (** [peek_opt cursor] immediately returns [Some (value, next)] with the
      [value] pushed to the position and a cursor to the [next] position, when
      the [cursor] points to a past position in the stream. Otherwise returns
      [None] or raises the exception that the stream was poisoned with. *)

  val read : 'a cursor -> 'a * 'a cursor
  (** [read cursor] immediately returns [(value, next)] with the [value] pushed
      to the position and a cursor to the [next] position, when the [cursor]
      points to a past position in the stream. If the [cursor] points to the
      current position of the stream, [read cursor] waits until a value is
      pushed to the stream or the stream is poisoned, in which case the
      exception that the stream was poisoned with will be raised. *)

  val read_evt : 'a cursor -> ('a * 'a cursor) Event.t
  (** [read_evt cursor] returns an {{!Event} event} that {{!read} reads} from
      the [cursor] position. *)
end

(** {1 Examples}

    {2 A simple bounded queue}

    Here is an example of a simple bounded (blocking) queue using a lock and
    condition variables:

    {[
      module Bounded_q : sig
        type 'a t

        val create : capacity:int -> 'a t
        val push : 'a t -> 'a -> unit
        val pop : 'a t -> 'a
      end = struct
        type 'a t = {
          lock : Lock.t;
          queue : 'a Queue.t;
          capacity : int;
          not_empty : Lock.Condition.t;
          not_full : Lock.Condition.t;
        }

        let create ~capacity =
          if capacity < 0 then invalid_arg "negative capacity"
          else
            let lock = Lock.create ()
            and queue = Queue.create ()
            and not_empty = Lock.Condition.create ()
            and not_full = Lock.Condition.create () in
            { lock; queue; capacity; not_empty; not_full }

        let is_full_unsafe t = t.capacity <= Queue.length t.queue

        let push t x =
          let was_empty =
            Lock.protect t.lock @@ fun () ->
            while is_full_unsafe t do
              Lock.Condition.wait t.not_full t.lock
            done;
            Queue.push x t.queue;
            Queue.length t.queue = 1
          in
          if was_empty then Lock.Condition.broadcast t.not_empty

        let pop t =
          let elem, was_full =
            Lock.protect t.lock @@ fun () ->
            while Queue.length t.queue = 0 do
              Lock.Condition.wait t.not_empty t.lock
            done;
            let was_full = is_full_unsafe t in
            (Queue.pop t.queue, was_full)
          in
          if was_full then Lock.Condition.broadcast t.not_full;
          elem
      end
    ]}

    The above is definitely not the fastest nor the most scalable bounded queue,
    but we can now demonstrate it with the cooperative {!Picos_mux_fifo}
    scheduler:

    {[
      # Picos_mux_fifo.run @@ fun () ->

        let bq =
          Bounded_q.create ~capacity:3
        in

        begin
          Flock.join_after ~on_return:`Terminate @@ fun () ->
          begin
            Flock.fork @@ fun () ->
            while true do
              Printf.printf "Popped %d\n%!"
                (Bounded_q.pop bq)
            done
          end;

          for i=1 to 5 do
            Printf.printf "Pushing %d\n%!" i;
            Bounded_q.push bq i
          done;

          Printf.printf "All done?\n%!";

          Control.yield ();
        end;

        Printf.printf "Pushing %d\n%!" 101;
        Bounded_q.push bq 101;

        Printf.printf "Popped %d\n%!"
          (Bounded_q.pop bq)
      Pushing 1
      Pushing 2
      Pushing 3
      Pushing 4
      Popped 1
      Popped 2
      Popped 3
      Pushing 5
      All done?
      Popped 4
      Popped 5
      Pushing 101
      Popped 101
      - : unit = ()
    ]}

    Notice how the producer was able to push three elements to the queue after
    which the fourth push blocked and the consumer was started. Also, after
    canceling the consumer, the queue could still be used just fine. *)

(** {1 Conventions}

    The optional [padded] argument taken by several constructor functions, e.g.
    {!Latch.create}, {!Mutex.create}, {!Condition.create},
    {!Semaphore.Counting.make}, and {!Semaphore.Binary.make}, defaults to
    [false]. When explicitly specified as [~padded:true] the object is allocated
    in a way to avoid
    {{:https://en.wikipedia.org/wiki/False_sharing} false sharing}. For
    relatively long lived objects this can improve performance and make
    performance more stable at the cost of using more memory. It is not
    recommended to use [~padded:true] for short lived objects.

    The primitives provided by this library are generally optimized for low
    contention scenariors and size. Generally speaking, for best performance and
    scalability, you should try to avoid high contention scenarios by
    architecting your program to distribute processing such that sequential
    bottlenecks are avoided. If high contention is unavoidable then other
    communication and synchronization primitive implementations may provide
    better performance. *)
