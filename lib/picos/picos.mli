(** Framework for interoperable effects based concurrency.

    This is essentially an interface between schedulers and other elements that
    need to communicate with a scheduler.  Perhaps an enlightening analogy is to
    say that this is the {{:https://en.wikipedia.org/wiki/POSIX} POSIX} of
    effects based schedulers.

    ℹ️ Picos, i.e. this module, is not intended to be an application level
    concurrent programming library or framework.  If you are looking for a
    library or framework for programming concurrent applications, then this
    module is probably not what you are looking for.

    {1 The architecture of Picos}

    The core concepts of Picos are

    - {!Trigger} — ability to await for a signal,
    - {!Computation} — a cancelable computation, and
    - {!Fiber} — an independent thread of execution,

    that are implemented in terms of the effects

    - {!Trigger.Await} — to suspend and resume a fiber,
    - {!Computation.Cancel_after} — to cancel a computation after given period
      of time,
    - {!Fiber.Current} — to obtain the unique handle of the current fiber,
    - {!Fiber.Yield} — to cooperatively request rescheduling the current fiber,
      and
    - {!Fiber.Spawn} — to start new fibers,

    that can be used to implement many kinds of higher level concurrent
    programming facilities.

    {1 Understanding cancelation}

    A central idea of Picos is to provide a collection of building blocks for
    parallelism safe cancelation.  Consider the following characteristic
    example:

    {@ocaml skip[
      Mutex.protect mutex begin fun () ->
        while true do
          Condition.wait condition mutex
        done
      end
    ]}

    Assume that the fiber executing the above computation might be canceled, at
    any point, by another fiber running in parallel.  How could that be done
    both effectively and safely?

    - To be effective, cancelation should take effect as soon as possible.  In
      this case, cancelation should take effect even during the
      {{!Picos_sync.Mutex.lock} [Mutex.lock]} inside
      {{!Picos_sync.Mutex.protect} [Mutex.protect]} and the
      {{!Picos_sync.Condition.wait} [Condition.wait]} operations when the fiber
      might be in a suspended state awaiting for a signal to continue.
    - To be safe, cancelation should not leave the program in an invalid state
      or cause the program to leak memory.  In this case, the ownership of the
      mutex must be transferred to the next fiber or be left unlocked and no
      references to unused objects must be left in the mutex or the condition
      variable.

    Picos allows {{!Picos_sync.Mutex} [Mutex]} and {{!Picos_sync.Condition}
    [Condition]} to be implemented such that cancelation may safely take effect
    at or during calls to {{!Picos_sync.Mutex.lock} [Mutex.lock]} and
    {{!Picos_sync.Condition.wait} [Condition.wait]}.

    {2 Cancelation in Picos}

    The {!Fiber} concept in Picos corresponds to an independent thread of
    execution.  A fiber may explicitly {{!Fiber.forbid} forbid} or
    {{!Fiber.permit} permit} the scheduler from propagating cancelation to it.
    This is important for the implementation of some key concurrent abstractions
    such as condition variables, where it is necessary to forbid cancelation
    when the associated mutex is reacquired.

    Each fiber has an associated {!Computation}.  A computation is something
    that needs to be completed either by {{!Computation.return} returning} a
    value through it or by {{!Computation.cancel} canceling} it with an
    exception.  To cancel a fiber one cancels the {{!Fiber.computation}
    computation} associated with the fiber.

    Before a computation has been completed, it is also possible to
    {{!Computation.try_attach} attach} a {!Trigger} to the computation and also
    to later {{!Computation.detach} detach} the trigger from the computation.  A
    trigger attached to a computation is {{!Trigger.signal} signaled} as the
    computation is completed.

    The {!Trigger} concept in Picos is what allows a fiber to be suspended and
    later resumed.  A fiber can create a trigger, add it to any shared data
    structure(s), and {{!Trigger.await} await} for the trigger to be signaled.
    The await operation, which is {{!Trigger.Await} implemented by the
    scheduler}, also, in case the fiber permits cancelation, attaches the
    trigger to the computation of the fiber when it suspends the fiber.  This is
    what allows a fiber to be resumed via cancelation of the computation.

    The return value of {{!Trigger.await} await} tells whether the fiber was
    resumed normally or due to being canceled and the caller then needs to
    properly handle either case.  After being canceled, depending on the
    concurrent abstraction being implemented, the caller might need to
    e.g. remove references to the trigger from the shared data structures,
    cancel asynchronous IO operations, or transfer ownership of a mutex to the
    next fiber in the queue of the mutex. *)

(** {1 Modules reference}

    We first open the {!Picos} module

    {[open Picos]}

    and define a simple scheduler for running the examples in this document on
    OCaml 4

    {@ocaml version<5.0.0[
      let run main =
        Picos_threaded.run ~forbid:false main
    ]}

    using {{!Picos_threaded} the basic thread based scheduler}
    and on OCaml 5

    {@ocaml version>=5.0.0[
      let run main =
        Picos_fifos.run ~forbid:false main
    ]}

    using {{!Picos_fifos} the basic effects based scheduler} that come with
    Picos as samples.

    {2 Auxiliary modules} *)

module Exn_bt = Picos_exn_bt
(** Exceptions with backtraces. *)

(** {2 Core modules}

    Please note that the example code snippets in this documentation may
    e.g. use the {!Domain} and {!Unix} modules in order to be able to describe
    Picos concepts in isolation in the absence of a Picos compatible
    scheduler. *)

module Trigger : sig
  (** Ability to await for a signal.

      To suspend and later resume the current thread of execution, one can
      {!create} a trigger, arrange {!signal} to be called on it, and {!await}
      for the call.

      Here is a simple example:

      {[
        run begin fun () ->
          let trigger = Trigger.create () in

          let signaler =
            Domain.spawn @@ fun () ->
              Trigger.signal trigger
          in
          let finally () =
            Domain.join signaler
          in
          Fun.protect ~finally @@ fun () ->

          match Trigger.await trigger with
          | None ->
            (* We were resumed normally. *)
            ()
          | Some exn_bt ->
            (* We were canceled. *)
            Exn_bt.raise exn_bt
        end
      ]}

      ⚠️ Typically we need to cleanup after {!await}, but in the above example we
      didn't insert the trigger into any data structure nor did we
      {{!Computation.try_attach} attach} the trigger to any computation.

      All operations on triggers are wait-free, with the obvious exception of
      {!await}.  The {!signal} operation inherits the properties of the action
      attached with {!on_signal} to the trigger. *)

  (** {2 Interface for suspending} *)

  type t
  (** Represents a trigger.  A trigger can be in one of three states: {i
      initial}, {i awaiting}, or {i signaled}.

      ℹ️ Once a trigger becomes signaled it no longer changes state.

      🏎️ A trigger in the initial and signaled states is a tiny object that does
      not hold onto any other objects. *)

  val create : unit -> t
  (** [create ()] allocates a new trigger in the initial state. *)

  val is_signaled : t -> bool
  (** [is_signaled trigger] determines whether the trigger is in the signaled
      state.

      This can be useful, for example, when a [trigger] is being inserted to
      multiple locations and might be signaled concurrently while doing so.  In
      such a case one can periodically check with [is_signaled trigger] whether
      it makes sense to continue.

      ℹ️ {!Computation.try_attach} already checks that the trigger being inserted
      has not been signaled so when attaching a trigger to multiple computations
      there is no need to separately check with [is_signaled]. *)

  val await : t -> Exn_bt.t option
  (** [await trigger] waits for the trigger to be {!signal}ed.

      The return value is [None] in case the trigger has been signaled and the
      {{!Fiber} fiber} was resumed normally.  Otherwise the return value is
      [Some exn_bt], which indicates that the fiber has been canceled and the
      caller should {{!Exn_bt.raise} raise} the exception.  In either case
      the caller is responsible for cleaning up.  Usually this means making sure
      that no references to the trigger remain to avoid space leaks.

      ⚠️ As a rule of thumb, if you inserted the trigger to some data structure
      or {{!Computation.try_attach} attached} it to some computation, then you
      are responsible for removing and {{!Computation.detach} detaching} the
      trigger after [await].

      ℹ️ A trigger in the signaled state only takes a small constant amount of
      memory.  Make sure that it is not possible for a program to accumulate
      unbounded numbers of signaled triggers under any circumstance.

      ⚠️ Only the owner or creator of a trigger may call [await].  It is
      considered an error to make multiple calls to [await].

      ℹ️ The behavior is that, {i unless [await] can return immediately},

      - on OCaml 5, [await] will perform the {!Await} effect, and
      - on OCaml 4, [await] will call the [await] operation of the {{!Handler}
        current handler}.

      @raise Invalid_argument if the trigger was in the awaiting state, which
        means that multiple concurrent calls of [await] are being made. *)

  (** {2 Interface for resuming} *)

  val signal : t -> unit
  (** [signal trigger] puts the [trigger] into the signaled state and calls
      the resume action, if any, attached using {!on_signal}.

      The intention is that calling [signal trigger] guarantees that any fiber
      {{!await} awaiting} the [trigger] will be resumed.  However, when and
      whether a fiber having called {!await} will be resumed normally or as
      canceled is determined by the scheduler that handles the {!Await} effect.

      ℹ️ Note that under normal circumstances, [signal] should never raise an
      exception.  If an exception is raised by [signal], it means that the
      handler of {!Await} has a bug or some catastrophic failure has
      occurred.

      ⚠️ Do not call [signal] from an effect handler in a scheduler. *)

  (** {2 Interface for schedulers} *)

  val is_initial : t -> bool
  (** [is_initial trigger] determines whether the trigger is in the initial
      or in the signaled state.

      ℹ️ Consider using {!is_signaled} instead of [is_initial] as in some
      contexts a trigger might reasonably be either in the initial or the
      awaiting state depending on the order in which things are being done.

      @raise Invalid_argument if the trigger was in the awaiting state. *)

  val on_signal : t -> 'x -> 'y -> (t -> 'x -> 'y -> unit) -> bool
  [@@alert
    handler
      "Only a scheduler should call this in the handler of the Await effect to \
       attach the scheduler specific resume action to the trigger.  Annotate \
       your effect handling function with [@alert \"-handler\"]."]
  (** [on_signal trigger x y resume] attempts to attach the [resume] action to
      the [trigger] and transition the trigger to the awaiting state.

      The return value is [true] in case the action was attached successfully.
      Otherwise the return value is [false], which means that the trigger was
      already in the signaled state.

      ⚠️ The action that you attach to a trigger must be safe to call from any
      context that might end up signaling the trigger directly or indirectly
      through {{!Computation.canceler} propagation}.  Unless you know, then you
      should assume that the [resume] action might be called from a different
      domain running in parallel with neither effect nor exception handlers and
      that if the attached action doesn't return the system may deadlock or if
      actions doesn't return quickly it may cause performance issues.

      ⚠️ It is considered an error to make multiple calls to [on_signal] with a
      specific [trigger].

      @raise Invalid_argument if the trigger was in the awaiting state, which
        means that either the owner or creator of the trigger made concurrent
        calls to {!await} or the handler called [on_signal] more than once. *)

  val from_action : 'x -> 'y -> (t -> 'x -> 'y -> unit) -> t
  [@@alert
    handler
      "This is an escape hatch for experts implementing schedulers or \
       structured concurrency mechanisms.  If you know what you are doing, use \
       [@alert \"-handler\"]."]
  (** [from_action x y resume] is equivalent to
      [let t = create () in assert (on_signal t x y resume); t].

      ⚠️ The action that you attach to a trigger must be safe to call from any
      context that might end up signaling the trigger directly or indirectly
      through {{!Computation.canceler} propagation}.  Unless you know, then you
      should assume that the [resume] action might be called from a different
      domain running in parallel with neither effect nor exception handlers and
      that if the attached action doesn't return the system may deadlock or if
      actions doesn't return quickly it may cause performance issues.

      ⚠️ The returned trigger will be in the awaiting state, which means that it
      is an error to call {!await}, {!on_signal}, or {!dispose} on it. *)

  val dispose : t -> unit
  (** [dispose trigger] transition the [trigger] from the initial state to the
      signaled state.

      🚦 The intended use case of [dispose] is for use from the handler of
      {!Await} to ensure that the trigger has been put to the signaled state
      after {!await} returns.

      @raise Invalid_argument if the trigger was in the awaiting state. *)

  include Intf.Trigger with type t := t with type exn_bt := Exn_bt.t

  (** {2 Design rationale}

      A key idea behind this design is that the handler for {!Await} does not
      need to run arbitrary user defined code while suspending a fiber: the
      handler calls {!on_signal} by itself.  This should make it easier to get
      both the handler and the user code correct.

      Another key idea is that the {!signal} operation provides no feedback as
      to the outcome regarding cancelation.  Calling {!signal} merely guarantees
      that the caller of {!await} will return.  This means that the point at
      which cancelation must be determined can be as late as possible.  A
      scheduler can check the cancelation status just before calling [continue]
      and it is, of course, possible to check the cancelation status earlier.
      This allows maximal flexibility for the handler of {!Await}.

      The consequence of this is that the only place to handle cancelation is at
      the point of {!await}.  This makes the design simpler and should make it
      easier for the user to get the handling of cancelation right.  A minor
      detail is that {!await} returns an option instead of raising an exception.
      The reason for this is that matching against an option is slightly faster
      than setting up an exception handler.  Returning an option also clearly
      communicates the two different cases to handle.

      On the other hand, the trigger mechanism does not have a way to specify a
      user-defined callback to perform cancelation immediately before the fiber
      is resumed.  Such an immediately called callback could be useful for e.g.
      canceling an underlying IO request.  One justification for not having such
      a callback is that cancelation is allowed to take place from outside of
      the scheduler, i.e. from another system level thread, and, in such a case,
      the callback could not be called immediately.  Instead, the scheduler is
      free to choose how to schedule canceled and continued fibers and, assuming
      that fibers can be trusted, a scheduler may give priority to canceled
      fibers.

      This design also separates the allocation of the atomic state for the
      trigger, or {!create}, from {!await}, and allows the state to be polled
      using {!is_signaled} before calling {!await}.  This is particularly useful
      when the trigger might need to be inserted to multiple places and be
      {!signal}ed in parallel before the call of {!await}.

      No mechanism is provided to communicate any result with the signal.  That
      can be done outside of the mechanism and is often not needed.  This
      simplifies the design.

      Once {!signal} has been called, a trigger no longer refers to any other
      object and takes just two words of memory.  This e.g. allows lazy removal
      of triggers, assuming the number of attached triggers can be bounded,
      because nothing except the trigger itself would be leaked.

      To further understand the problem domain, in this design, in a
      suspend-resume scenario, there are three distinct pieces of state:

      {ol

        {- The state of shared data structure(s) used for communication and / or
        synchronization.}

        {- The state of the trigger.}

        {- The cancelation status of the fiber.}}

      The trigger and cancelation status are both updated independently and
      atomically through code in this framework.  The key requirement left for
      the user is to make sure that the state of the shared data structure is
      updated correctly independently of what {!await} returns.  So, for
      example, a mutex implementation must check, after getting [Some exn_bt],
      what the state of the mutex is and how it should be updated. *)
end

module Computation : sig
  (** A cancelable computation.

      A computation basically holds the status, i.e.

      - running,
      - returned, or
      - canceled,

      of some sort of computation.

      A hopefully enlightening analogy is that a computation is a kind of
      single-shot atomic event.

      Another hopefully helpful analogy is that a computation is basically like
      a cancelable promise and a basic non-cancelable promise can be implemented
      trivially on top of a computation.

      To define a computation, one first {{!create} creates} it and then
      arranges for the computation to be completed by {{!return} returning} a
      value through it or by {{!cancel} canceling} it with an exception at some
      point in the future.  There are no restrictions on what it means for a
      computation to be running.  The cancelation status of a computation can be
      polled or {{!check} checked} explicitly.  Observers can also
      {{!try_attach} attach} {{!Trigger}triggers} to a computation to get a
      signal when the computation is completed or {{!await} await} the
      computation.

      Here is an example:

      {[
        run begin fun () ->
          let computation =
            Computation.create ()
          in
          let computer =
            Domain.spawn @@ fun () ->
              let rec fib i =
                Computation.check computation;
                if i <= 1 then
                  i
                else
                  fib (i - 1) + fib (i - 2)
              in
              Computation.capture computation
                fib 10
          in
          let finally () =
            Domain.join computer
          in
          Fun.protect ~finally @@ fun () ->

          let canceler =
            Domain.spawn @@ fun () ->
              Unix.sleepf 0.1;
              Computation.cancel computation
              @@ Exn_bt.get_callstack 2 Exit
          in
          let finally () =
            Domain.join canceler
          in
          Fun.protect ~finally @@ fun () ->

          Computation.await computation
        end
      ]}

      In this framework, a fiber is always associated with {{!Fiber.computation}
      at least a single computation}.  However, {{!Fiber.spawn} it is possible
      for multiple fibers to share a single computation} and it is also possible
      for a single fiber to perform multiple computations.

      Computations are not hierarchical.  In other words, computations do not
      directly implement structured concurrency.  However, it is possible to
      {{!canceler} propagate cancelation} to implement structured concurrency on
      top of computations.

      Operations on computations are either wait-free or lock-free and designed
      to avoid starvation and complete in amortized constant time.  The
      properties of operations to complete a computation depend on the
      properties of actions {{!Trigger.on_signal} attached} to the triggers. *)

  (** {2 Interface for creating} *)

  type !'a t
  (** Represents a cancelable computation.  A computation is either {i running}
      or has been {i completed} either with a return value or with canceling
      {{!Exn_bt} exception with a backtrace}.

      ℹ️ Once a computation becomes completed it no longer changes state.

      🏎️ A computation that has been completed is a small object that only holds
      onto the return value or the canceling exception with a backtrace.

      ⚠️ In the running state a computation may refer to any number of
      {{!Trigger} triggers} and it is important to make sure that any triggers
      {{!try_attach} attached} to a computation are {{!detach} detached} when
      they are no longer needed unless the computation has been completed. *)

  val create : ?mode:[ `FIFO | `LIFO ] -> unit -> 'a t
  (** [create ()] creates a new computation in the running state.

      The optional [mode] specifies the order in which {{!Trigger} triggers}
      {{!try_attach} attached} to the computation will be {{!Trigger.signal}
      signaled} after the computation has been completed.  [`FIFO] ordering may
      reduce latency of IO bound computations and is the default.  [`LIFO] may
      improve thruput of CPU bound computations and be preferable on a
      work-stealing scheduler, for example.

      ℹ️ Typically the creator of a computation object arranges for the
      computation to be completed by using the {!capture} helper, for example.
      However, it is possible and safe to race multiple threads of execution to
      complete a computation. *)

  val finished : unit t
  (** [finished] is a constant finished computation. *)

  val try_return : 'a t -> 'a -> bool
  (** [try_return computation value] attempts to complete the computation with
      the specified [value] and returns [true] on success.  Otherwise returns
      [false], which means that the computation had already been completed
      before. *)

  val return : 'a t -> 'a -> unit
  (** [return computation value] is equivalent to
      [try_return computation value |> ignore]. *)

  val try_finish : unit t -> bool
  (** [try_finish computation] is equivalent to [try_return computation ()]. *)

  val finish : unit t -> unit
  (** [finish computation] is equivalent to
      [try_finish computation |> ignore]. *)

  val try_capture : 'a t -> ('b -> 'a) -> 'b -> bool
  (** [try_capture computation fn x] calls [fn x] and tries to complete the
      computation with the value returned or the exception raised by the call
      and returns [true] on success.  Otherwise returns [false], which means
      that the computation had already been completed before. *)

  val capture : 'a t -> ('b -> 'a) -> 'b -> unit
  (** [capture computation fn x] is equivalent to
      [try_capture computation fn x |> ignore]. *)

  (** {2 Interface for canceling} *)

  (** An existential wrapper for computations. *)
  type packed = Packed : 'a t -> packed

  val try_cancel : 'a t -> Exn_bt.t -> bool
  (** [try_cancel computation exn_bt] attempts to mark the computation as
      canceled with the specified exception and backtrace and returns [true] on
      success.  Otherwise returns [false], which means that the computation had
      already been completed before. *)

  val cancel : 'a t -> Exn_bt.t -> unit
  (** [cancel computation exn_bt] is equivalent to
      [try_cancel computation exn_bt |> ignore]. *)

  (** {2 Interface for timeouts} *)

  val cancel_after : 'a t -> seconds:float -> Exn_bt.t -> unit
  (** [cancel_after ~seconds computation exn_bt] arranges to {!cancel} the
      computation after the specified time with the specified exception and
      backtrace.  Completion of the computation before the specified time
      effectively cancels the timeout.

      ℹ️ The behavior is that [cancel_after] first checks that [seconds] is not
      negative, and then

      - on OCaml 5, [cancel_after] will perform the {!Cancel_after} effect, and
      - on OCaml 4, [cancel_after] will call the [cancel_after] operation of the
        {{!Handler} current handler}.

      @raise Invalid_argument if [seconds] is negative or too large as
        determined by the scheduler. *)

  (** {2 Interface for polling} *)

  val is_running : 'a t -> bool
  (** [is_running computation] determines whether the computation is in the
      running state meaning that it has not yet been completed. *)

  val is_canceled : 'a t -> bool
  (** [is_canceled computation] determines whether the computation is in the
      canceled state. *)

  val canceled : 'a t -> Exn_bt.t option
  (** [canceled computation] returns the exception that the computation has been
      canceled with or returns [None] in case the computation has not been
      canceled. *)

  val check : 'a t -> unit
  (** [check computation] is equivalent to
      [Option.iter Exn_bt.raise (canceled computation)]. *)

  val peek : 'a t -> ('a, Exn_bt.t) result option
  (** [peek computation] returns the result of the computation or [None] in case
      the computation has not completed. *)

  (** {2 Interface for awaiting} *)

  val try_attach : 'a t -> Trigger.t -> bool
  (** [try_attach computation trigger] tries to attach the trigger to be
      signaled on completion of the computation and returns [true] on success.
      Otherwise returns [false], which means that the computation has already
      been completed or the trigger has already been signaled.

      ⚠️ Always {!detach} a trigger after it is no longer needed unless the
      computation is known to have been completed. *)

  val detach : 'a t -> Trigger.t -> unit
  (** [detach computation trigger] {{!Trigger.signal} signals} the trigger and
      detaches it from the computation.

      🏎️ The {!try_attach} and [detach] operations essentially implement a
      lock-free bag.  While not formally wait-free, the implementation is
      designed to avoid starvation by making sure that any potentially expensive
      operations are performed cooperatively. *)

  val await : 'a t -> 'a
  (** [await computation] waits for the computation to complete and either
      returns the value of the completed computation or raises the exception the
      computation was canceled with.

      ℹ️ If the computation has already completed, then [await] returns or raises
      immediately without performing any effects. *)

  val wait : _ t -> unit
  (** [wait computation] waits for the computation to complete. *)

  (** {2 Interface for propagating cancelation} *)

  val canceler : from:_ t -> into:_ t -> Trigger.t
  (** [canceler ~from ~into] creates a trigger that propagates cancelation
      [from] one computation [into] another on {{:Trigger.signal} signal}.  The
      returned trigger is not attached to any computation.

      The returned trigger is usually attached to the computation [from] which
      cancelation is to be propagated and the trigger should usually also be
      detached after it is no longer needed.

      The intended use case of [canceler] is as a low level building block of
      structured concurrency mechanisms.  Picos does not require concurrent
      programming models to be hierarchical or structured.

      ⚠️ The returned trigger will be in the awaiting state, which means that it
      is an error to call {!Trigger.await} or {!Trigger.on_signal} on it. *)

  (** {2 Interface for schedulers} *)

  include Intf.Computation with type 'a t := 'a t with type exn_bt := Exn_bt.t

  val with_action :
    ?mode:[ `FIFO | `LIFO ] ->
    'x ->
    'y ->
    (Trigger.t -> 'x -> 'y -> unit) ->
    'a t
  [@@alert
    handler
      "This is an escape hatch for experts implementing schedulers or \
       structured concurrency mechanisms.  If you know what you are doing, use \
       [@alert \"-handler\"]."]
  (** [with_action x y resume] is equivalent to
      {@ocaml skip[
        let computation = create () in
        let trigger = Trigger.from_action x y resume in
        let _ : bool = try_attach computation trigger in
        computation
      ]}

      ⚠️ The same warnings as with {!Trigger.from_action} apply. *)

  (** {2 Design rationale}

      The computation concept can be seen as a kind of single-shot atomic event
      that is a generalization of both a cancelation context or token and of a
      promise.  Unlike a typical promise mechanism, a computation can be
      canceled.  Unlike a typical cancelation mechanism, a computation can and
      should also be completed in case it is not canceled.  This promotes proper
      scoping of computations and resource cleanup at completion, which is how
      the design evolved from a more traditional cancelation context design.

      In this framework, {{!Fiber.computation} every fiber has an associated
      computation}.  Being able to return a value through the computation means
      that no separate promise is necessarily required to hold the result of a
      fiber.  On the other hand, in this framework, {{!Fiber.spawn} multiple
      fibers may share a single computation}.  This allows multiple fibers to be
      canceled efficiently through a single atomic update.  In other words, the
      design allows various higher level patterns to be implemented efficiently.

      Instead of directly implementing a hierarchy of computations, the design
      allows {{!try_attach} attach}ing triggers to computations and
      {{!canceler}a special trigger constructor} is provided for propagating
      cancelation.  This helps to keep the implementation lean, i.e. not
      substantially heavier than a typical promise implementation.

      Finally, just like with {!Trigger.Await}, a key idea is that the handler
      of {!Computation.Cancel_after} does not need to run arbitrary user defined
      code.  The action of any trigger attached to a computation either comes
      from some scheduler calling {!Trigger.on_signal} or from
      {!Computation.canceler}. *)
end

module Fiber : sig
  (** An independent thread of execution.

      A fiber corresponds to an independent thread of execution.  Fibers are
      {!create}d by schedulers in response to {!Spawn} effects.  A fiber is
      associated with a {{!Computation} computation} and either {!forbid}s or
      {!permit}s the scheduler from propagating cancelation to it.  A fiber also
      has an associated {{!FLS} fiber local storage}.

      ⚠️ Many operations on fibers can only be called safely from the fiber
      itself, because those operations are neither concurrency nor parallelism
      safe.  Such operations can be safely called from a handler in a scheduler
      when it is handling an effect performed by the fiber.  In particular, a
      scheduler can safely check whether the fiber {!has_forbidden} cancelation
      and may access the {!FLS} of the fiber. *)

  (** {2 Interface for rescheduling} *)

  val yield : unit -> unit
  (** [yield ()] asks the current fiber to be rescheduled.

      ℹ️ The behavior is that

      - on OCaml 5, [yield] perform the {!Yield} effect, and
      - on OCaml 4, [yield] will call the [yield] operation of the {{!Handler}
        current handler}. *)

  (** {2 Interface for spawning} *)

  val spawn : forbid:bool -> 'a Computation.t -> (unit -> unit) list -> unit
  (** [spawn ~forbid computation mains] starts new fibers by performing the
      {!Spawn} effect.  The fibers will share the same [computation] and start
      with {{!Fiber.has_forbidden} propagation of cancelation forbidden or
      permitted} depending on the [forbid] flag.

      ℹ️ Any {{!Computation} computation}, including the computation of the
      current fiber, may be passed as the computation for new fibers.  Higher
      level libraries are free to implement the desired structuring principles.

      ⚠️ Behavior is undefined if any function in [mains] raises an exception.
      For example, raising an exception might terminate the whole application
      (recommended, but not required) or the exception might be ignored.  In
      other words, the caller {i must} arrange for the computation to be
      completed and errors reported in a desired manner.

      ℹ️ The behavior is that

      - on OCaml 5, [spawn] performs the {!Spawn} effect, and
      - on OCaml 4, [spawn] will call the [spawn] operation of the {{!Handler}
        current handler}. *)

  (** {2 Interface for current fiber} *)

  type t
  (** Represents a fiber or an independent thread of execution.

      ⚠️ Unlike with most other concepts of Picos, operations on fibers are
      typically {i not} concurrency or parallelism safe, because the fiber is
      considered to be owned by a single thread of execution. *)

  type fiber := t
  (** Type alias for {!Fiber.t} within this signature. *)

  val current : unit -> t
  (** [current ()] returns the current fiber.

      ⚠️ Extra care should be taken when storing the fiber object in any shared
      data structure, because, aside from checking whether two fibers are
      {!equal}, or from accessing the associated {!computation}, it is generally
      unsafe to perform any operations on foreign fibers.

      ℹ️ The behavior is that

      - on OCaml 5, [current] performs the {!Current} effect, and
      - on OCaml 4, [current] will call the [current] operation of the
        {{!Handler} current handler}.

      ⚠️ The [current] operation must always resume the fiber without propagating
      cancelation.  A scheduler may, of course, decide to reschedule the current
      fiber to be resumed later. *)

  val has_forbidden : t -> bool
  (** [has_forbidden fiber] determines whether the fiber {!forbid}s or
      {!permit}s the scheduler from propagating cancelation to it.

      ℹ️ This is mostly useful in the effect handlers of schedulers.

      ⚠️ There is no "reference count" of how many times a fiber has forbidden or
      permitted propagation of cancelation.  Calls to {!forbid} and {!permit}
      directly change a single boolean flag.

      ⚠️ It is only safe to call [has_forbidden] from the fiber itself. *)

  val forbid : t -> (unit -> 'a) -> 'a
  (** [forbid fiber thunk] tells the scheduler that cancelation must not be
      propagated to the fiber during the execution of [thunk].

      The main use case of [forbid] is the implementation of concurrent
      abstractions that may have to {{!Trigger.await} await} for something, or
      may need to perform other effects, and must not be canceled while doing
      so.  For example, the wait operation on a condition variable typically
      reacquires the associated mutex before returning, which may require
      awaiting for the owner of the mutex to release it.

      ℹ️ [forbid] does not prevent the fiber or the associated {!computation}
      from being canceled.  It only tells the scheduler not to propagate
      cancelation to the fiber.

      ⚠️ It is only safe to call [forbid] from the fiber itself. *)

  val permit : t -> (unit -> 'a) -> 'a
  (** [permit fiber thunk] tells the scheduler that cancelation may be
      propagated to the fiber during the execution of [thunk].

      It is possible to {!spawn} a fiber with cancelation forbidden, which means
      that cancelation won't be propagated to fiber unless it is explicitly
      {{!permit} permitted} by the fiber at some point.

      ⚠️ It is only safe to call [permit] from the fiber itself. *)

  val is_canceled : t -> bool
  (** [is_canceled fiber] is equivalent to
      {@ocaml skip[
        not (Fiber.has_forbidden fiber) &&
        let (Packed computation) =
          Fiber.computation fiber
        in
        Computation.is_canceled computation
      ]}

      ℹ️ This is mostly useful in the effect handlers of schedulers.

      ⚠️ It is only safe to call [is_canceled] from the fiber itself. *)

  val canceled : t -> Exn_bt.t option
  (** [canceled fiber] is equivalent to:
      {@ocaml skip[
        if Fiber.has_forbidden fiber then
          None
        else
          let (Packed computation) =
            Fiber.computation fiber
          in
          Computation.canceled computation
      ]}

      ℹ️ This is mostly useful in the effect handlers of schedulers.

      ⚠️ It is only safe to call [canceled] from the fiber itself. *)

  val check : t -> unit
  (** [check fiber] is equivalent to:
      {@ocaml skip[
        if not (Fiber.has_forbidden fiber) then
          let (Packed computation) =
            Fiber.computation fiber
          in
          Computation.check computation
      ]}

      ℹ️ This is mostly useful for periodically polling the cancelation status
      during CPU intensive work.

      ⚠️ It is only safe to call [check] from the fiber itself. *)

  val exchange : t -> forbid:bool -> bool
  (** [exchange fiber ~forbid] sets the bit that tells the scheduler whether to
      propagate cancelation or not and returns the previous state. *)

  val set : t -> forbid:bool -> unit
  (** [set fiber ~forbid] sets the bit that tells the scheduler whether to
      propagate cancelation or not. *)

  module FLS : sig
    (** Fiber local storage

        Fiber local storage is intended for use as a low overhead storage
        mechanism for fiber extensions.  For example, one might associate a
        priority value with each fiber for a scheduler that uses a priority
        queue or one might use FLS to store unique id values for fibers.

        Here is an example of how one might define a fiber id:

        {[
          let fiber_id_key =
            let next = Atomic.make 0 in
            Fiber.FLS.new_key
            @@ Computed (fun () ->
                Atomic.fetch_and_add next 1)
        ]}

        Here is an example of how one might define a fiber priority:

        {[
          let fiber_priority_key =
            Fiber.FLS.new_key (Constant 0)
        ]}

        A priority based scheduler could then use {!get} to access the priority
        of a fiber highly efficiently.  Fibers that are fine with the default
        priority do not necessarily need to store a priority at all. *)

    type !'a key
    (** Represents a key for storing values of type ['a] in storage associated
        with fibers. *)

    (** Type to specify initial values for fibers. *)
    type 'a initial = Constant of 'a | Computed of (unit -> 'a)

    val new_key : 'a initial -> 'a key
    (** [new_key initial] allocates a new key for associating values in storage
        associated with fibers.  The [initial] value for every fiber is either
        the given {!Constant} or is {!Computed} with the given function.  If the
        initial value is a constant, no value needs to be stored unless the
        value is explicitly updated.

        ⚠️ New keys should not be created dynamically. *)

    val get : t -> 'a key -> 'a
    (** [get fiber key] returns the value associated with the [key] in the
        storage associated with the [fiber].

        ⚠️ It is only safe to call [get] from the fiber itself. *)

    val set : t -> 'a key -> 'a -> unit
    (** [set fiber key value] sets the [value] associated with the [key] to the
        given value in the storage associated with the [fiber].

        ⚠️ It is only safe to call [set] from the fiber itself. *)
  end

  (** {2 Interface for foreign fiber} *)

  val equal : t -> t -> bool
  (** [equal fiber1 fiber2] is physical equality for fibers, i.e. it determines
      whether [fiber1] and [fiber2] are one and the same fiber.

      ℹ️ One use case of [equal] is in the implementation of concurrent
      primitives like mutexes where it makes sense to check that acquire and
      release operations are performed by the same fiber. *)

  val computation : t -> Computation.packed
  (** [computation fiber] returns the computation that the fiber has been
      {!create}d with. *)

  val try_attach : t -> Trigger.t -> bool
  (** [try_attach fiber trigger] is equivalent to
      [let Packed c = computation fiber in Computation.try_attach c trigger]. *)

  val detach : t -> Trigger.t -> unit
  (** [detach fiber trigger] is equivalent to
      [let Packed c = computation fiber in Computation.detach c trigger]. *)

  module Maybe : sig
    (** An unboxed optional {{!Fiber.t} fiber}. *)

    type t
    (** Either a {{!Fiber.t} fiber} or {!nothing}. *)

    val nothing : t
    (** Not a fiber. *)

    val of_fiber : fiber -> t
    (** [of_fiber fiber] casts the fiber into an optional fiber.

        🏎️ This performs no allocations. *)

    val current_if : bool option -> t
    (** [current_if checked] returns {!nothing} in case [checked] is
        [Some false] and otherwise [of_fiber (Fiber.current ())]. *)

    val current_and_check_if : bool option -> t
    (** [current_check_if checked] returns {!nothing} in case [checked] is
        [Some false] and otherwise [of_fiber (Fiber.current ())] and also calls
        {!Fiber.check} on the fiber. *)

    val or_current : t -> t
    (** [or_current maybe] returns [of_fiber (Fiber.current ())] in case [maybe]
        is {!nothing} and otherwise returns [maybe]. *)

    val to_fiber_or_current : t -> fiber
    (** [to_fiber_or_current maybe] returns [Fiber.current ()] in case [maybe]
        is {!nothing} and otherwise returns the fiber that [maybe] was cast
        from. *)

    val check : t -> unit
    (** [check maybe] returns immediately if [maybe] is [nothing] and otherwise
        calls {!Fiber.check} on the fiber. *)

    val equal : t -> t -> bool
    (** [equal l r] determines whether [l] and [r] are maybe equal.
        Specifically, if either [l] or [r] or both is {!nothing}, then they are
        considered (maybe) equal.  Otherwise [l] and [r] are compared for
        {{!Fiber.equal} physical equality}. *)

    val unequal : t -> t -> bool
    (** [equal l r] determines whether [l] and [r] are maybe unequal.
        Specifically, if either [l] or [r] or both is {!nothing}, then they are
        considered (maybe) unequal.  Otherwise [l] and [r] are compared for
        {{!Fiber.equal} physical equality}. *)

    (** {2 Design rationale}

        The fiber identity is often needed only for the purpose of dynamically
        checking against programming errors.  Unfortunately it can be relative
        expensive to obtain the {{!Fiber.current} current} fiber.

        As a data point, in a benchmark that increments an [int ref] protected
        by a mutex, obtaining the fiber identity for the lock and unlock
        operations — that only need it for error checking purposes — roughly
        tripled the cost of an increment on a machine.

        Using GADTs internally allows an optional fiber to be provided without
        adding overhead to operations on non-optional fibers and allows optional
        fibers to used without allocations at a very low cost. *)
  end

  (** {2 Interface for schedulers} *)

  val create : forbid:bool -> 'a Computation.t -> t
  (** [create ~forbid computation] creates a new fiber. *)

  val try_suspend :
    t -> Trigger.t -> 'x -> 'y -> (Trigger.t -> 'x -> 'y -> unit) -> bool
  (** [try_suspend fiber trigger x y resume] tries to suspend the [fiber] to
      await for the [trigger] to be {{!Trigger.signal} signaled}.  If the result
      is [false], then the [trigger] is guaranteed to be in the signaled state
      and the fiber should be eventually resumed.  If the result is [true], then
      the fiber was suspended, meaning that the [trigger] will have had the
      [resume] action {{!Trigger.on_signal} attached} to it and the trigger has
      potentially been {{!Computation.try_attach} attached} to the
      {!computation} of the fiber. *)

  val unsuspend : t -> Trigger.t -> bool
  (** [unsuspend fiber trigger] makes sure that the [trigger] will not be
      attached to the computation of the [fiber].  Returns [false] in case the
      fiber has been canceled and propagation of cancelation is not forbidden.
      Otherwise returns [true].

      ⚠️ The trigger must be in the signaled state! *)

  include
    Intf.Fiber
      with type t := t
      with type 'a computation := 'a Computation.t
      with type exn_bt := Exn_bt.t

  (** {2 Design rationale}

      The idea is that fibers correspond 1-to-1 with independent threads of
      execution.  This allows a fiber to non-atomically store state related to a
      thread of execution.

      The status of whether propagation of cancelation is forbidden or permitted
      could be stored in the {{!FLS} fiber local storage}.  The justification
      for storing it directly with the fiber is that the implementation of some
      key synchronization and communication mechanisms, such as condition
      variables, requires the capability.

      No integer fiber id is provided by default.  It would seem that for most
      intents and purposes the identity of the fiber is sufficient.  {{!FLS}
      Fiber local storage} can be used to implement a fiber id or e.g. a fiber
      hash.

      The {{!FLS} fiber local storage} is designed for the purpose of extending
      fibers and to be as fast as possible.  It is not intended for application
      programming.

      {!Yield} is provided as a separate effect to specifically communicate the
      intent that the current fiber should be rescheduled.  This allows all the
      other effect handlers more freedom in choosing which fiber to schedule
      next. *)
end

module Handler : sig
  (** Handler for the effects based operations of Picos for OCaml 4. *)

  type 'c t = {
    current : 'c -> Fiber.t;  (** See {!Picos.Fiber.current}. *)
    spawn :
      'a. 'c -> forbid:bool -> 'a Computation.t -> (unit -> unit) list -> unit;
        (** See {!Picos.Fiber.spawn}. *)
    yield : 'c -> unit;  (** See {!Picos.Fiber.yield}. *)
    cancel_after :
      'a. 'c -> 'a Computation.t -> seconds:float -> Exn_bt.t -> unit;
        (** See {!Picos.Computation.cancel_after}. *)
    await : 'c -> Trigger.t -> Exn_bt.t option;
        (** See {!Picos.Trigger.await}. *)
  }
  (** A record of implementations of the primitive effects based operations of
      Picos.  The operations take a context of type ['c] as an argument. *)

  val using : 'c t -> 'c -> (unit -> 'a) -> 'a
  (** [using handler context thunk] sets the [handler] and the [context] for the
      handler of the primitive effects based operations of Picos while running
      [thunk].

      ℹ️ The behavior is that

      - on OCaml 4, [using] stores the [handler] in {{!Picos_thread.TLS} [TLS]},
        which allows the operations to be accessed during the execution of the
        [thunk], and
      - on OCaml 5, [using] runs [thunk] with a deep effect handler that
        delegates to the operations of the [handler].

      ⚠️ While this works on OCaml 5, you usually want to use a scheduler that
      implements an effect handler directly, because that is likely to perform
      better. *)
end
