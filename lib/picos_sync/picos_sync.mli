(** Basic communication and synchronization primitives for {!Picos}.

    This library essentially provides a conventional set of communication and
    synchronization primitives for concurrent programming with any Picos
    compatible scheduler.

    For the {{!examples} examples} we open some modules:

    {[
      open Picos_structured
      open Picos_sync
    ]} *)

open Picos

(** {1 Modules} *)

module Mutex : sig
  (** A mutual-exclusion lock or mutex.

      ℹ️ This intentionally mimics the interface of {!Stdlib.Mutex}.  Unlike with
      the standard library mutex, blocking on this mutex potentially allows an
      effects based scheduler to run other fibers on the thread.

      🏎️ The optional [checked] argument taken by most of the operations defaults
      to [true].  When explicitly specified as [~checked:false] the mutex
      implementation may avoid having to obtain the {{!Picos.Fiber.current}
      current fiber}, which can be expensive relative to locking or unlocking an
      uncontested mutex.  Note that specifying [~checked:false] on an operation
      may prevent error checking also on a subsequent operation. *)

  type t
  (** Represents a mutual-exclusion lock or mutex. *)

  val create : ?padded:bool -> unit -> t
  (** [create ()] returns a new mutex that is initially unlocked. *)

  val lock : ?checked:bool -> t -> unit
  (** [lock mutex] locks the [mutex].

      ℹ️ If the fiber has been canceled and propagation of cancelation is
      allowed, this may raise the cancelation exception before locking the
      mutex.  If [~checked:false] was specified, the cancelation exception may
      or may not be raised.

      @raise Sys_error if the mutex is already locked by the fiber.  If
        [~checked:false] was specified for some previous operation on the mutex
        the exception may or may not be raised. *)

  val try_lock : ?checked:bool -> t -> bool
  (** [try_lock mutex] locks the mutex in case the mutex is unlocked.  Returns
      [true] on success and [false] in case the mutex was locked.

      ℹ️ If the fiber has been canceled and propagation of cancelation is
      allowed, this may raise the cancelation exception before locking the
      mutex.  If [~checked:false] was specified, the cancelation exception may
      or may not be raised. *)

  val unlock : ?checked:bool -> t -> unit
  (** [unlock mutex] unlocks the mutex.

      @raise Sys_error if the mutex was locked by another fiber.  If
        [~checked:false] was specified for some previous operation on the mutex
        the exception may or may not be raised. *)

  val protect : ?checked:bool -> t -> (unit -> 'a) -> 'a
  (** [protect mutex thunk] locks the [mutex], runs [thunk ()], and unlocks the
      [mutex] after [thunk ()] returns or raises.

      ℹ️ If the fiber has been canceled and propagation of cancelation is
      allowed, this may raise the cancelation exception before locking the
      mutex.  If [~checked:false] was specified, the cancelation exception may
      or may not be raised.

      @raise Sys_error for the same reasons as {!lock} and {!unlock}. *)
end

module Condition : sig
  (** A condition variable.

      ℹ️ This intentionally mimics the interface of {!Stdlib.Condition}.  Unlike
      with the standard library condition variable, blocking on this condition
      variable allows an effects based scheduler to run other fibers on the
      thread. *)

  type t
  (** Represents a condition variable. *)

  val create : ?padded:bool -> unit -> t
  (** [create ()] return a new condition variable. *)

  val wait : t -> Mutex.t -> unit
  (** [wait condition] unlocks the [mutex], waits for the [condition], and locks
      the [mutex] before returning or raising due to the operation being
      canceled.

      ℹ️ If the fiber has been canceled and propagation of cancelation is
      allowed, this may raise the cancelation exception. *)

  val signal : t -> unit
  (** [signal condition] wakes up one fiber waiting on the [condition] variable
      unless there are no such fibers. *)

  val broadcast : t -> unit
  (** [broadcast condition] wakes up all the fibers waiting on the [condition]
      variable. *)
end

module Semaphore : sig
  (** {!Counting} and {!Binary} semaphores.

      ℹ️ This intentionally mimics the interface of {!Stdlib.Semaphore}.  Unlike
      with the standard library semaphores, blocking on these semaphores allows
      an effects based scheduler to run other fibers on the thread. *)

  module Counting : sig
    (** A counting semaphore. *)

    type t
    (** Represents a counting semaphore. *)

    val make : ?padded:bool -> int -> t
    (** [make initial] creates a new counting semaphore with the given [initial]
        count.

        @raise Invalid_argument in case the given [initial] count is negative. *)

    val release : t -> unit
    (** [release semaphore] increments the count of the semaphore.

        @raise Sys_error in case the count would overflow. *)

    val acquire : t -> unit
    (** [acquire semaphore] waits until the count of the semaphore is greater
        than [0] and then atomically decrements the count. *)

    val try_acquire : t -> bool
    (** [try_acquire semaphore] attempts to atomically decrement the count of
        the semaphore unless the count is already [0]. *)

    val get_value : t -> int
    (** [get_value semaphore] returns the current count of the semaphore.  This
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
    (** [release semaphore] sets the count of the semaphore to [1]. *)

    val acquire : t -> unit
    (** [acquire semaphore] waits until the count of the semaphore is [1] and
        then atomically changes the count to [0]. *)

    val try_acquire : t -> bool
    (** [try_acquire semaphore] attempts to atomically change the count of the
        semaphore from [1] to [0]. *)
  end
end

module Lazy : sig
  (** A lazy suspension.

      ℹ️ This intentionally mimics the interface of {!Stdlib.Lazy}.  Unlike with
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
      suspension and reproduces its result.  In case the suspension has already
      been forced the computation is skipped and stored result is reproduced.

      ℹ️ This will check whether the current fiber has been canceled before
      starting the computation of [thunk ()].  This allows the suspension to be
      forced by another fiber.  However, if the fiber is canceled and the
      cancelation exception is raised after the computation has been started,
      the suspension will then store the cancelation exception.

      @raise Undefined in case the suspension is currently being forced by the
        {{!Picos.Fiber.current} current} fiber. *)

  val force_val : 'a t -> 'a
  (** [force_val] is a synonym for {!force}. *)

  val map : ('a -> 'b) -> 'a t -> 'b t
  (** [map fn susp] is equivalent to
      {{!from_fun} [from_fun (fun () -> fn (force susp))]}. *)

  val map_val : ('a -> 'b) -> 'a t -> 'b t
  (** [map_val fn susp] is equivalent to:
      {@ocaml skip[
        if is_val susp then
          from_val (fn (force susp))
        else
          map fn susp
      ]} *)
end

module Event : sig
  (** First-class synchronous communication abstraction.

      Events describe a thing that might happen in the future, or a concurrent
      offer or request that might be accepted or succeed, but is cancelable if
      some other event happens first.

      See the {!Picos_select} library for an example.

      ℹ️ This module intentionally mimics the
      {{:https://ocaml.org/manual/5.2/api/Event.html} [Event]} module provided
      by the OCaml POSIX threads library. *)

  type !'a t
  (** An event returning a value of type ['a]. *)

  type 'a event = 'a t
  (** An alias for the {!Event.t} type to match the
      {{:https://ocaml.org/manual/5.2/api/Event.html} [Event]} module
      signature. *)

  val always : 'a -> 'a t
  (** [always value] returns an event that can always be committed to resulting
      in the given [value]. *)

  (** {2 Composing events} *)

  val choose : 'a t list -> 'a t
  (** [choose events] return an event that offers all of the given events and
      then commits to at most one of them. *)

  val wrap : 'b t -> ('b -> 'a) -> 'a t
  (** [wrap event fn] returns an event that acts as the given [event] and then
      applies the given function to the value in case the event is committed
      to. *)

  val map : ('b -> 'a) -> 'b t -> 'a t
  (** [map fn event] is equivalent to {{!wrap} [wrap event fn]}. *)

  val guard : (unit -> 'a t) -> 'a t
  (** [guard thunk] returns an event that, when {{!sync} synchronized}, calls
      the [thunk], and then behaves like the resulting event.

      ⚠️ Raising an exception from a [guard thunk] will result in raising that
      exception out of the {!sync}.  This may result in dropping the result of
      an event that committed just after the exception was raised.  This means
      that you should treat an unexpected exception raised from {!sync} as a
      fatal error. *)

  (** {2 Consuming events} *)

  val sync : 'a t -> 'a
  (** [sync event] synchronizes on the given event.

      Synchronizing on an event executes in three phases:

      {ol
        {- In the first phase offers or requests are made to communicate.}
        {- One of the offers or requests is committed to and all the other
           offers and requests are canceled.}
        {- A final result is computed from the value produced by the event.}}

      ⚠️ [sync event] does not wait for the canceled concurrent requests to
      terminate.  This means that you should arrange for guaranteed cleanup
      through other means such as the use of {{!Picos_structured} structured
      concurrency}. *)

  val select : 'a t list -> 'a
  (** [select events] is equivalent to {{!sync} [sync (choose events)]}. *)

  (** {2 Primitive events}

      ℹ️ The {{!Picos.Computation} [Computation]} concept of {!Picos} can be seen
      as a basic single-shot atomic event.  This module builds on that concept
      to provide a composable API to concurrent services exposed through
      computations. *)

  open Picos

  type 'a request = {
    request : 'r. (unit -> 'r) Computation.t -> ('a -> 'r) -> unit;
  }
  [@@unboxed]
  (** Represents a function that requests a concurrent service to update a
      {{!Picos.Computation} computation}.

      ⚠️ Raising an exception from a [request] function will result in raising
      that exception out of the {!sync}.  This may result in dropping the result
      of an event that committed just after the exception was raised.  This
      means that you should treat an unexpected exception raised from {!sync} as
      a fatal error.  In addition, you should arrange for concurrent services to
      report unexpected errors independently of the computation being passed to
      the service. *)

  val from_request : 'a request -> 'a t
  (** [from_request { request }] creates an {{!Event} event} from the request
      function. *)

  val from_computation : 'a Computation.t -> 'a t
  (** [from_computation source] creates an {{!Event} event} that can be
      committed to once the given [source] computation has completed.

      ℹ️ Committing to some other event does not cancel the [source]
      computation. *)
end

module Latch : sig
  (** A dynamic single-use countdown latch.

      Latches are typically used for determining when a finite set of parallel
      computations is done.  If the size of the set is known a priori, then the
      latch can be initialized with the size as initial count and then each
      computation just {{!decr} decrements} the latch.

      If the size is unknown, i.e. it is determined dynamically, then a latch is
      initialized with a count of one, the a priori known computations are
      started and then the latch is {{!decr} decremented}.  When a computation
      is stsrted, the latch is {{!try_incr} incremented}, and then {{!decr}
      decremented} once the computation has finished. *)

  type t
  (** Represents a dynamic countdown latch. *)

  val create : ?padded:bool -> int -> t
  (** [create initial] creates a new countdown latch with the specified
      [initial] count.

      @raise Invalid_argument in case the specified [initial] count is
        negative. *)

  val try_decr : t -> bool
  (** [try_decr latch] attempts to decrement the count of the latch and returns
      [true] in case the count of the latch was greater than zero and [false] in
      case the count already was zero. *)

  val decr : t -> unit
  (** [decr latch] is equivalent to:
      {@ocaml skip[
        if not (try_decr latch) then
          invalid_arg "zero count"
      ]}

      @raise Invalid_argument in case the count of the latch is zero. *)

  val try_incr : t -> bool
  (** [try_incr latch] attempts to increment the count of the latch and returns
      [true] on success and [false] on failure, which means that the latch has
      already reached zero. *)

  val incr : t -> unit
  (** [incr latch] is equivalent to:
      {@ocaml skip[
        if not (try_incr latch) then
          invalid_arg "zero count"
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
      incremental variable.  Returns [true] on success and [false] in case the
      variable had already been poisoned or assigned a value. *)

  val fill : 'a t -> 'a -> unit
  (** [fill ivar value] is equivalent to
      {{!try_fill} [try_fill ivar value |> ignore]}. *)

  val try_poison : 'a t -> Exn_bt.t -> bool
  (** [try_poison ivar exn_bt] attempts to poison the incremental variable with
      the specified exception and backtrace.  Returns [true] on success and
      [false] in case the variable had already been poisoned or assigned a
      value. *)

  val poison : 'a t -> Exn_bt.t -> unit
  (** [poison ivar exn_bt] is equivalent to
      {{!try_poison} [try_poison ivar exn_bt |> ignore]}. *)

  val peek_opt : 'a t -> 'a option
  (** [peek_opt ivar] either returns [Some value] in case the variable has been
      assigned the [value], raises an exception in case the variable has been
      poisoned, or otherwise returns [None], which means that the variable has
      not yet been poisoned or assigned a value. *)

  val read : 'a t -> 'a
  (** [read ivar] waits until the variable is either assigned a value or the
      variable is poisoned and then returns the value or raises the
      exception. *)

  val read_evt : 'a t -> 'a Event.t
  (** [read_evt ivar] returns an event that can be committed to once the
      variable has either been assigned a value or has been poisoned. *)
end

module Stream : sig
  (** A lock-free, poisonable, many-to-many, stream.

      Readers can {!tap} into a stream to get a {!cursor} for reading all the
      values {{!push} pushed} to the stream starting from the {!cursor}
      position.  Conversely, values {{!push} pushed} to a stream are lost unless
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

  val poison : 'a t -> Exn_bt.t -> unit
  (** [poison stream exn_bt] marks the stream as poisoned at the current
      position, which means that subsequent attempts to {!push} to the [stream]
      will raise the given exception with backtrace. *)

  type !'a cursor
  (** Represents a (past or current) position in a stream. *)

  val tap : 'a t -> 'a cursor
  (** [tap stream] returns a {!cursor} to the current position of the
      [stream]. *)

  val peek_opt : 'a cursor -> ('a * 'a cursor) option
  (** [peek_opt cursor] immediately returns [Some (value, next)] with the
      [value] pushed to the position and a cursor to the [next] position, when
      the [cursor] points to a past position in the stream.  Otherwise returns
      [None] or raises the exception that the stream was poisoned with. *)

  val read : 'a cursor -> 'a * 'a cursor
  (** [read cursor] immediately returns [(value, next)] with the [value] pushed
      to the position and a cursor to the [next] position, when the [cursor]
      points to a past position in the stream.  If the [cursor] points to the
      current position of the stream, [read cursor] waits until a value is
      pushed to the stream or the stream is poisoned, in which case the
      exception that the stream was poisoned with will be raised. *)

  val read_evt : 'a cursor -> ('a * 'a cursor) Event.t
  (** [read_evt cursor] returns an {{!Event} event} that {{!read} reads} from
      the [cursor] position. *)
end

(** {1 Examples}

    {2 A simple bounded queue}

    Here is an example of a simple bounded (blocking) queue using a mutex and
    condition variables:

    {[
      module Bounded_q : sig
        type 'a t
        val create : capacity:int -> 'a t
        val push : 'a t -> 'a -> unit
        val pop : 'a t -> 'a
      end = struct
        type 'a t = {
          mutex : Mutex.t;
          queue : 'a Queue.t;
          capacity : int;
          not_empty : Condition.t;
          not_full : Condition.t;
        }

        let create ~capacity =
          if capacity < 0 then
            invalid_arg "negative capacity"
          else {
            mutex = Mutex.create ();
            queue = Queue.create ();
            capacity;
            not_empty = Condition.create ();
            not_full = Condition.create ();
          }

        let is_full_unsafe t =
          t.capacity <= Queue.length t.queue

        let push t x =
          let was_empty =
            Mutex.protect t.mutex @@ fun () ->
            while is_full_unsafe t do
              Condition.wait t.not_full t.mutex
            done;
            Queue.push x t.queue;
            Queue.length t.queue = 1
          in
          if was_empty then
            Condition.broadcast t.not_empty

        let pop t =
          let elem, was_full =
            Mutex.protect t.mutex @@ fun () ->
            while Queue.length t.queue = 0 do
              Condition.wait
                t.not_empty t.mutex
            done;
            let was_full = is_full_unsafe t in
            Queue.pop t.queue, was_full
          in
          if was_full then
            Condition.broadcast t.not_full;
          elem
      end
    ]}

    The above is definitely not the fastest nor the most scalable bounded queue,
    but we can now demonstrate it with the cooperative {!Picos_fifos} scheduler:

    {[
      # Picos_fifos.run @@ fun () ->

        let bq =
          Bounded_q.create ~capacity:3
        in

        Flock.join_after begin fun () ->
          Flock.fork begin fun () ->
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

          Flock.terminate ()
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
    which the fourth push blocked and the consumer was started.  Also, after
    canceling the consumer, the queue could still be used just fine. *)

(** {1 Conventions}

    The optional [padded] argument taken by several constructor functions, e.g.
    {!Latch.create}, {!Mutex.create}, {!Condition.create},
    {!Semaphore.Counting.make}, and {!Semaphore.Binary.make}, defaults to
    [false].  When explicitly specified as [~padded:true] the object is
    allocated in a way to avoid {{:https://en.wikipedia.org/wiki/False_sharing}
    false sharing}.  For relatively long lived objects this can improve
    performance and make performance more stable at the cost of using more
    memory.  It is not recommended to use [~padded:true] for short lived
    objects. *)
