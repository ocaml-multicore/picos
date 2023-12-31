(** {1 Picos — Interoperable effects based concurrency}

    {2 Introduction}

    Picos, or {{:https://en.wikipedia.org/wiki/Metric_prefix} pico}-scheduler
    framework, is a framework for building
    {{:https://en.wikipedia.org/wiki/Interoperability}interoperable} elements of
    {{:https://v2.ocaml.org/manual/effects.html} effects based}
    {{:https://en.wikipedia.org/wiki/Cooperative_multitasking} cooperative}
    {{:https://en.wikipedia.org/wiki/Concurrent_computing} concurrent
    programming models}.  Such models include elements such as

    - {{:https://en.wikipedia.org/wiki/Scheduling_(computing)} schedulers} that
      multiplex large numbers of {{:https://en.wikipedia.org/wiki/Green_thread}
      user level fibers} to run on a small number of system level threads,
    - mechanisms for managing fibers and for
      {{:https://en.wikipedia.org/wiki/Structured_concurrency} structuring
      concurrency},
    - communication and synchronization primitives, such as
      {{:https://en.wikipedia.org/wiki/Monitor_(synchronization)} mutexes and
      condition variables}, message queues,
      {{:https://en.wikipedia.org/wiki/Software_transactional_memory} STMs}, and
      more, and
    - integration with low level
      {{:https://en.wikipedia.org/wiki/Asynchronous_I/O} asynchronous IO}
      systems.

    Picos is not intended to be an application level concurrent programming
    library or framework.  If you are looking for a library or framework for
    programming concurrent applications, then Picos is probably not what you are
    looking for.

    If you are the author of an application level concurrent programming library
    or framework, then Picos should not fundamentally be competing with your
    work.  However, Picos and libraries built on top of Picos probably do have
    overlap with your work and making your work Picos compatible may offer
    benefits:

    - You may find it useful that Picos provides parallelism safe building
      blocks for cancelation, which is a particularly tricky problem to get
      right.
    - You may find it useful that you don't have to reinvent many of the basic
      communication and synchronization abstractions such as mutexes and
      condition variables, promises, concurrent bounded queues, channels, and
      what not.
    - You may benefit from further non-trivial libraries, such as IO libraries,
      that you don't have to reimplement.
    - Potential users of your work may be reassured and benefit from the ability
      to mix-and-match your work with other Picos compatible libraries and
      frameworks.

    Of course, interoperability does have some costs.  It takes time to
    understand Picos and it takes time to implement Picos compatibility.
    Implementing your programming model elements in terms of Picos primitives
    may not give ideal results.  To address concerns such as those, a conscious
    effort has been made to keep Picos as minimal and unopinionated as
    possible.

    {3 Understanding cancelation}

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
      this case, cancelation should take effect even during the [Mutex.lock]
      inside [Mutex.protect] and the [Condition.wait] operations when the fiber
      might be in a suspended state awaiting for a signal to continue.
    - To be safe, cancelation should not leave the program in an invalid state
      or cause the program to leak memory.  In this case, the ownership of the
      mutex must be transferred to the next fiber or be left unlocked and no
      references to unused objects must be left in the mutex or the condition
      variable.

    Picos allows [Mutex] and [Condition] to be implemented such that cancelation
    may safely take effect at or during calls to [Mutex.lock] and
    [Condition.wait].

    {4 Cancelation in Picos}

    The {!Fiber} concept in Picos corresponds to an independent thread of
    execution.  A fiber may explicitly {{!Fiber.forbid} forbid} or
    {{!Fiber.permit} permit} the scheduler from propagating cancelation to it.
    This is important for the implementation of some key concurrent abstractions
    such as condition variables, where it is necessary to forbid cancelation
    when the associated mutex is re-acquired.

    Each fiber has an associated {!Computation}.  A computation is something
    that needs to be completed either by {{!Computation.return} returning} a
    value through it or by {{!Computation.cancel}canceling} it with an
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
    next fiber in the queue of the mutex.

    {3 The architecture of Picos}

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

    {4 Picos compatible}

    While Picos provides OCaml 4 compatible default behavior for the effects,
    the idea is that in OCaml 5 effects based schedulers provide their own
    handlers for the effects.  By handling the Picos effects a scheduler becomes
    Picos compatible and allows any libraries built on top of Picos to be used
    with the scheduler.

    {4 Implemented in Picos}

    A scheduler is just one element of a concurrent programming model.
    Separately from making a scheduler Picos compatible, one may choose to
    implement other elements of the programming model, e.g. a particular
    approach to structuring concurrency or a particular collection of
    communication and synchronization primitives, in terms of the Picos
    primitives.  Such elements can then be used on any Picos compatible
    scheduler.

    {3 Design goals and principles}

    - {b Simple}: Picos should be kept as simple as possible.
    - {b Minimal}: Picos should be kept minimal.  The dependency footprint
      should be as small as possible.  Convenience features should be built on
      top of the framework.
    - {b Safe}: Picos should be designed with safety in mind.  The
      implementation must be data race free.  The framework should promote and
      always allow proper resource management.
    - {b Unopinionated}: Picos should not make strong design choices that are
      controversial.
    - {b Flexible}: Picos should allow higher level libraries as much freedom as
      possible to make their own design choices.

    The documentation of the concepts includes design rationale for some of the
    specific ideas behind their detailed design.

    {4 Constraints Liberate, Liberties Constrain}

    Picos aims to be unopinionated and flexible enough to allow higher level
    libraries to provide many different kinds of concurrent programming models.
    While it is impossible to give a complete list of what Picos does not
    dictate, it is perhaps illuminating to explicitly mention some of those:

    - Picos does not implement
      {{:https://en.wikipedia.org/wiki/Capability-based_security}
      capability-based security}.  Higher level libraries with or without
      capabilities may be built on top of Picos.
    - Picos never cancels computations implicitly.  Higher level libraries may
      decide when cancelation should be allowed to take effect.
    - Picos does not dictate which fiber should be scheduled next after a Picos
      effect.  Different schedulers may freely use desired data structures
      (queues, work-stealing deques, stacks, priority queues, ...) and, after
      handling any Picos effect, freely decide which fiber to run next.
    - Picos does not dictate how fibers should be managed.  It is possible to
      implement both unstructured and structured concurrent programming models
      on top of Picos.
    - Picos does not dictate which mechanisms applications should use for
      communication and synchronization.  It is possible to build many different
      kinds of communication and synchronization mechanisms on top of Picos
      including mutexes and condition variables, STMs, asynchronous and
      synchronous message passing, {{:https://en.wikipedia.org/wiki/Actor_model}
      actors}, and more.
    - Picos does not dictate that there should be a connection between the
      scheduler and other elements of the concurrent programming model.  It is
      possible to provide those separately and mix-and-match.
    - Picos does not dictate which library to use for IO.  It is possible to
      build direct-style asynchronous IO libraries on top of Picos that can then
      be used with any Picos compatible schedulers or concurrent programming
      models.

    Let's build an incredible ecosystem of interoperable concurrent programming
    libraries and frameworks! *)

(** {2 Modules reference}

    {[open Picos]}

    {3 Auxiliary modules} *)

module DLS : sig
  (** Auxiliary module implementing domain-local storage.

      On OCaml 4 there is always only a single domain. *)

  type 'a key
  (** Represents a key for storing values of type ['a] in storage associated
      with domains. *)

  val new_key : (unit -> 'a) -> 'a key
  (** [new_key compute] allocates a new key for associating values in storage
      associated with domains.  The initial value for each domain is [compute]d
      by calling the given function if the [key] is {{!get}read} before it has
      been {{!set}written}.  The [compute] function might be called multiple
      times per domain, but only one result will be used. *)

  val get : 'a key -> 'a
  (** [get key] returns the value associated with the [key] in the storage
      associated with the current domain. *)

  val set : 'a key -> 'a -> unit
  (** [set key value] sets the [value] associated with the [key] in the storage
      associated with the current domain. *)
end

module TLS : sig
  (** Auxiliary module implementing thread-local storage.

      Note that here "thread" refers to system level threads rather than fibers.
      In case system level thread implementation, i.e. the [threads.posix]
      library, is not available, this will use {!DLS}. *)

  type 'a key
  (** Represents a key for storing values of type ['a] in storage associated
      with threads. *)

  val new_key : (unit -> 'a) -> 'a key
  (** [new_key compute] allocates a new key for associating values in storage
      associated with threads.  The initial value for each thread is [compute]d
      by calling the given function if the [key] is {{!get}read} before it has
      been {{!set}written}. *)

  val get : 'a key -> 'a
  (** [get key] returns the value associated with the [key] in the storage
      associated with the current thread. *)

  val set : 'a key -> 'a -> unit
  (** [set key value] sets the [value] associated with the [key] in the storage
      associated with the current thread. *)
end

module Exn_bt : sig
  (** Auxiliary module for exceptions with backtraces *)

  type t = { exn : exn; bt : Printexc.raw_backtrace }
  (** An exception and a backtrace. *)

  val get : exn -> t
  (** [get exn] is equivalent to
      [{ exn; bt = Printexc.get_raw_backtrace () }]. *)

  val get_callstack : int -> exn -> t
  (** [get_callstack n exn] is equivalent to
      [{ exn; bt = Printexc.get_callstack n }].

      Note that [Printexc.get_callstack 0] effectively returns a constant value
      and this function is optimized to take that into account. *)

  val raise : t -> 'a
  (** [raise exn_bt] is equivalent to
      [Printexc.raise_with_backtrace exn_bt.exn exn_bt.bt]. *)

  include Effects_intf.Exn_bt with type t := t
end

(** {3 Core modules}

    Please note that the example code snippets in this documentation may
    e.g. use the [Domain] and [Unix] modules in order to be able to describe
    Picos concepts in isolation in the absence of a Picos compatible
    scheduler. *)

module Trigger : sig
  (** Ability to await for a signal

      To suspend and later resume the current thread of execution, one can
      {!create} a trigger, arrange {!signal} to be called on it, and {!await}
      for the call.

      Here is a simple example:

      {[
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
          (* We were canceled.

             Typically we'd cleanup here. *)
          Exn_bt.raise exn_bt
      ]}

      All operations on triggers are wait-free, with the obvious exception of
      {!await}.  The {!signal} operation inherits the properties of the action
      attached with {!on_signal} to the trigger. *)

  (** {2 Interface for suspending} *)

  type -'allowed t
  (** Represents a trigger. *)

  val create : unit -> [ `Await | `Signal ] t
  (** [create ()] allocates a new trigger in the initial state. *)

  val is_initial : _ t -> bool
  (** [is_initial trigger] determines whether the trigger is in the initial
      state.

      ⚠️ Consider using {!is_signaled} instead of [is_initial] as in some
      contexts a trigger might reasonably be either in the initial or the
      awaiting state depending on the order in which things are being done. *)

  val is_signaled : _ t -> bool
  (** [is_signaled trigger] determines whether the trigger is in the signaled
      state. *)

  val await : [> `Await ] t -> Exn_bt.t option
  (** [await trigger] waits for the trigger to be {!signal}ed.

      The return value is [None] in case the trigger was signaled before [await]
      or the {{!Fiber} fiber} was resumed normally.  Otherwise the return value
      is [Some exn_bt], which indicates that the fiber has been canceled and the
      caller should raise the exception.  In either case the caller is
      responsible for cleaning up.  Usually this means making sure that no
      references to the trigger remain to avoid space leaks.

      ⚠️ Only the owner or creator of a trigger may call [await] and it is
      considered an error to make multiple concurrent calls to [await].

      On OCaml 5, [await] will first try to perform the {!Await} effect and
      falls back to the OCaml 4 default implementation that suspends the
      underlying system level thread using a per thread [Mutex] and [Condition]
      variable.

      @raise Invalid_argument if the trigger was in the awaiting state, which
        means that multiple concurrent calls of [await] are being made. *)

  (** {2 Interface for resuming} *)

  type as_signal = [ `Signal ] t
  (** Synonym for a trigger that only allows the {!signal} operation. *)

  val signal : [> `Signal ] t -> unit
  (** After [signal trigger] returns, the trigger has been put into the signaled
      state and any attached action has been called.

      Note that under normal circumstances, [signal] should never raise an
      exception.  If an exception is raised by [signal], it means that the
      handler of {!Await} has a bug or some catastrophic failure has
      occurred. *)

  (** {2 Interface for schedulers} *)

  val on_signal :
    [> `On | `Signal ] t ->
    'x ->
    'y ->
    ([> `Signal ] t -> 'x -> 'y -> unit) ->
    bool
  (** [on_signal trigger x y resume] attempts to attach the [resume] action to
      the [trigger] and transition the trigger to the awaiting state.  It must
      be safe to call [resume trigger x y] from any context that {!signal} might
      be called from.

      The return value is [true] in case the action was attached successfully.
      Otherwise the return value is [false], which means that the trigger was
      already in the signaled state.

      ⚠️ The handler of {!Await} should call [on_signal] at most once.

      @raise Invalid_argument if the trigger was in the awaiting state, which
        means that either the owner or creator of the trigger made concurrent
        calls to {!await} or the handler called [on_signal] more than once. *)

  include
    Effects_intf.Trigger with type 'a t := 'a t with type exn_bt := Exn_bt.t

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
      of triggers.

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
  (** A cancelable computation

      A computation simply holds the status, i.e.

      - running,
      - returned, or
      - canceled,

      of some sort of computation.

      As a hopefully helpful analogy, a computation is basically like a
      cancelable promise and a basic non-cancelable promise can be implemented
      trivially on top of a computation.

      To define a computation, one first {!create}s it and then arranges for the
      computation to be completed by {!return}ing a value through it or by
      {!cancel}ing it with an exception at some point in the future.  There are
      no restrictions on what it means for a computation to be running.  The
      cancelation status of a computation can be {!check}ed explicitly.  Outside
      observers can {{!try_attach} attach} {{!Trigger}triggers} to a computation
      to get a signal when the computation is completed or {!await} the
      computation.  Outside observers may also be selectively given the ability
      to {!cancel} a computation.

      Here is an example:

      {[
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

  type (!'a, -'allowed) t
  (** Represents a cancelable computation. *)

  val create : unit -> ('a, [ `Await | `Cancel | `Return ]) t
  (** [create ()] creates a new computation in the running state. *)

  val finished : (unit, [ `Await | `Cancel | `Return ]) t
  (** [finished] is a constant finished computation. *)

  val return : ('a, [> `Return ]) t -> 'a -> unit
  (** [return computation value] attempts to complete the computation with the
      specified [value]. *)

  val finish : (unit, [> `Return ]) t -> unit
  (** [finish computation] is equivalent to [return computation ()]. *)

  val capture : ('a, [> `Cancel | `Return ]) t -> ('b -> 'a) -> 'b -> unit
  (** [capture computation fn x] calls [fn x] and tries to complete the
      computation with the value returned or the exception raised by the
      call. *)

  val cancel_after :
    ('a, [> `Await | `Cancel ]) t -> seconds:float -> Exn_bt.t -> unit
  (** [cancel_after ~seconds computation exn_bt] arranges to {!cancel} the
      computation after the specified time with the specified exception and
      backtrace.  Completion of the computation before the specified time
      effectively cancels the timeout.

      On OCaml 5, [cancel_after] will first try to perform the {!Cancel_after}
      effect and falls back to the OCaml 4 default implementation using
      [Unix.select] and a background [Thread] when available.

      @raise Invalid_argument if [seconds] is negative. *)

  (** {2 Interface for canceling} *)

  type 'a as_cancelable = ('a, [ `Await | `Cancel ]) t
  (** Synonym for a computation that only allows cancelation aside from
      await. *)

  (** An existential wrapper for computations. *)
  type -'allowed packed = Packed : ('a, 'allowed) t -> 'allowed packed

  type packed_as_cancelable = [ `Await | `Cancel ] packed
  (** Synonym for a packed computation that only allows cancelation aside from
      await. *)

  val cancel : ('a, [> `Cancel ]) t -> Exn_bt.t -> unit
  (** [cancel computation exn_bt] attempts to mark the computation as canceled
      with the specified exception and backtrace. *)

  (** {2 Interface for polling} *)

  val is_running : ('a, _) t -> bool
  (** [is_running computation] determines whether the computation is in the
      running state meaning that it has not yet been completed. *)

  val canceled : ('a, _) t -> Exn_bt.t option
  (** [canceled computation] returns the exception that the computation has been
      canceled with or returns [None] in case the computation has not been
      canceled. *)

  val check : ('a, _) t -> unit
  (** [check computation] is equivalent to
      [Option.iter Exn_bt.raise (canceled computation)]. *)

  val peek : ('a, _) t -> ('a, Exn_bt.t) result option
  (** [peek computation] returns the result of the computation or [None] in case
      the computation has not completed. *)

  (** {2 Interface for awaiting} *)

  val try_attach : ('a, [> `Await ]) t -> [> `Signal ] Trigger.t -> bool
  (** [try_attach computation trigger] tries to attach the trigger to be
      signaled on completion of the computation and returns [true] on success.
      Otherwise returns [false], which means that the computation has already
      been completed or the trigger has already been signaled. *)

  val detach : ('a, [> `Await ]) t -> [> `Signal ] Trigger.t -> unit
  (** [detach computation trigger] {{!Trigger.signal} signals} the trigger and
      detaches it from the computation. *)

  val await : ('a, [> `Await ]) t -> 'a
  (** [await computation] waits for the computation to complete and either
      returns the value of the completed computation or raises the exception the
      computation was canceled with. *)

  (** {2 Interface for propagating cancelation} *)

  val canceler :
    from:('a, [> `Await ]) t ->
    into:('b, [> `Cancel ]) t ->
    [ `Signal ] Trigger.t
  (** [canceler ~from ~into] creates a trigger that propagates cancelation
      [from] one computation [into] another on {{:Trigger.signal} signal}.

      The returned trigger is usually attached to the computation [from] which
      cancelation is to be propagated and the trigger should usually also be
      detached after it is no longer needed. *)

  (** {2 Interface for schedulers} *)

  include
    Effects_intf.Computation
      with type 'a as_cancelable := 'a as_cancelable
      with type exn_bt := Exn_bt.t

  (** {2 Design rationale}

      The computation concept can be seen as a generalization of both a
      cancelation context or token and of a promise.  Unlike a typical promise
      mechanism, a computation can be canceled.  Unlike a typical cancelation
      mechanism, a computation can and should also be completed in case it is
      not canceled.  This promotes proper scoping of computations and resource
      cleanup at completion, which is how the design evolved from a more
      traditional cancelation context design.

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
  (** An independent thread of execution

      A fiber corresponds to an independent thread of execution.  Fibers are
      {!create}d by schedulers in response to {!Spawn} effects.  A fiber is
      associated with a {{!Computation} computation} and either {!forbid}s or
      {!permit}s the scheduler from propagating cancelation to it.  A fiber also
      has an associated {{!FLS} fiber local storage}. *)

  (** {2 Interface for rescheduling} *)

  val yield : unit -> unit
  (** [yield ()] asks the current fiber to be re-scheduled.

      On OCaml 5, [yield] will first attempt to perform the {!Yield} effect and
      falls back to the OCaml 4 default implementation calling [Thread.yield ()]
      when available. *)

  (** {2 Interface for spawning} *)

  val spawn :
    forbid:bool ->
    ('a, [> `Await | `Cancel ]) Computation.t ->
    (unit -> unit) list ->
    unit
  (** [spawn ~forbid computation mains] starts new fibers by performing the
      {!Spawn} effect.  The fibers will share the same [computation] and start
      with {{!Fiber.has_forbidden} propagation of cancelation forbidden or
      permitted} depending on the [forbid] flag.

      Note that any computation, including the computation of the current fiber,
      may be passed as the computation for new fibers.  Higher level libraries
      are free to implement the desired structuring principles.

      ⚠️ Behavior is undefined if any function in [mains] raises an exception.
      For example, raising an exception might terminate the whole application
      (recommended, but not required) or the exception might be ignored.  In
      other words, the caller {i must} arrange for the computation to be
      completed and errors reported in a desired manner.

      On OCaml 5, [spawn] will first try to perform the {!Spawn} effect and
      falls back to the OCaml 4 default implementation creating [Thread]s when
      available. *)

  (** {2 Interface for current fiber} *)

  type -'allowed t
  (** Represents a fiber. *)

  val current : unit -> [ `Sync | `Async ] t
  (** [current ()] returns the current fiber.

      On OCaml 5, [current] will first try to perform the {!Current} effect and
      falls back to the OCaml 4 default implementation using [Thread]-local
      storage. *)

  (** ⚠️ Operations that require the fiber handle to be [[> `Sync]] can only be
      called safely from the fiber itself. *)

  val has_forbidden : [> `Sync ] t -> bool
  (** [has_forbidden fiber] determines whether the fiber {!forbid}s or
      {!permit}s the scheduler from propagating cancelation to it. *)

  val forbid : [> `Sync ] t -> (unit -> 'a) -> 'a
  (** [forbid fiber thunk] tells the scheduler that cancelation must not be
      propagated to the fiber during the execution of [thunk].

      Note that this does not prevent the associated {!computation} from being
      canceled.  This only tells the scheduler not to propagate cancelation to
      the fiber. *)

  val permit : [> `Sync ] t -> (unit -> 'a) -> 'a
  (** [permit fiber thunk] tells the scheduler that cancelation may be
      propagated to the fiber during the execution of [thunk]. *)

  val canceled : [> `Sync ] t -> Exn_bt.t option
  (** [canceled fiber] is equivalent to:
      {@ocaml skip[
        if Fiber.has_forbidden fiber then
          None
        else
          let (Packed computation) =
            Fiber.computation fiber
          in
          Computation.canceled computation
      ]} *)

  val check : [> `Sync ] t -> unit
  (** [check fiber] is equivalent to:
      {@ocaml skip[
        if not (Fiber.has_forbidden fiber) then
          let (Packed computation) =
            Fiber.computation fiber
          in
          Computation.check computation
      ]} *)

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
            let key =
              Fiber.FLS.new_key
              @@ Computed (fun () ->
                  Atomic.fetch_and_add next 1)
            in
            (key :> _ Fiber.FLS.as_read_only)
        ]}

        Here is an example of how one might define a fiber priority:

        {[
          let fiber_priority_key =
            Fiber.FLS.new_key (Constant 0)
        ]}

        A priority based scheduler could then use {!get} to access the priority
        of a fiber highly efficiently.  Fibers that are fine with the default
        priority do not necessarily need to store a priority at all. *)

    type (!'a, -'allowed) key
    (** Represents a key for storing values of type ['a] in storage associated
        with fibers. *)

    type 'a as_read_only = ('a, [ `Get ]) key
    (** Synonym for a key with allowed operations restricted to reading. *)

    (** Type to specify initial values for fibers. *)
    type 'a initial = Constant of 'a | Computed of (unit -> 'a)

    val new_key : 'a initial -> ('a, [ `Get | `Set ]) key
    (** [new_key initial] allocates a new key for associating values in storage
        associated with fibers.  The [initial] value for every fiber is either
        the given {!Constant} or is {!Computed} with the given function.  If the
        initial value is a constant, no value needs to be stored unless the
        value is explicitly updated.

        ⚠️ New keys should not be created dynamically. *)

    (** ⚠️ Operations that require the fiber handle to be [[> `Sync]] can only be
        called safely from the fiber itself. *)

    val get : [> `Sync ] t -> ('a, [> `Get ]) key -> 'a
    (** [get fiber key] returns the value associated with the [key] in the
        storage associated with the [fiber]. *)

    val set : [> `Sync ] t -> ('a, [> `Set ]) key -> 'a -> unit
    (** [set fiber key value] sets the [value] associated with the [key] to the
        given value in the storage associated with the [fiber]. *)
  end

  (** {2 Interface for foreign fiber} *)

  type as_async = [ `Async ] t
  (** Synonym for a fiber with the allowed operations restricted to those that
      can be performed from outside of the fiber. *)

  val equal : 'allowed t -> 'allowed t -> bool
  (** [equal fiber1 fiber2] determines whether [fiber1] and [fiber2] are one and
      the same fiber. *)

  val computation : [> `Async ] t -> Computation.packed_as_cancelable
  (** [computation fiber] returns the computation that the fiber has been
      {!create}d with. *)

  val try_attach : [> `Async ] t -> [> `Signal ] Trigger.t -> bool
  (** [try_attach fiber trigger] is equivalent to
      [let Packed c = computation fiber in Computation.try_attach c trigger]. *)

  val detach : [> `Async ] t -> [> `Signal ] Trigger.t -> unit
  (** [detach fiber trigger] is equivalent to
      [let Packed c = computation fiber in Computation.detach c trigger]. *)

  (** {2 Interface for schedulers} *)

  val create :
    forbid:bool ->
    ('a, [> `Await | `Cancel ]) Computation.t ->
    [ `Sync | `Async ] t
  (** [create ~forbid computation] creates a new fiber. *)

  include
    Effects_intf.Fiber
      with type 'a t := 'a t
      with type 'a as_cancelable := 'a Computation.as_cancelable

  (** {2 Design rationale}

      The idea is that fibers correspond 1-to-1 with independent threads of
      execution.  This allows a fiber to non-atomically store state related to a
      thread of execution.

      The status of whether propagation of cancelation is forbidden or permitted
      could be stored in the {{!FLS}fiber local storage}.  The justification for
      storing it directly with the fiber is that the implementation of some key
      synchronization and communication mechanisms, such as condition variables,
      requires the capability.

      No integer fiber id is provided by default.  It would seem that for most
      intents and purposes the identity of the fiber is sufficient.
      {{!FLS}Fiber local storage} can be used to implement a fiber id or e.g. a
      fiber hash.

      The {{!FLS}fiber local storage} is designed for the purpose of extending
      fibers and to be as fast as possible.  It is not intended for application
      programming.

      {!Yield} is provided as a separate effect to specifically communicate the
      intent that the current fiber should be rescheduled.  This allows all the
      other effect handlers more freedom in choosing which fiber to schedule
      next. *)
end

(** {2 Advanced topics}

    {3 Default behaviors}

    All of the effects based operations

    - {!Trigger.await},
    - {!Computation.cancel_after},
    - {!Fiber.current},
    - {!Fiber.yield}, and
    - {!Fiber.spawn}

    have OCaml 4 compatible default behaviors or defaults in Picos.  Those
    defaults allow libraries implemented in Picos to work out-of-the-box without
    a scheduler.

    The primary audience for the defaults is casual users such as those who are
    not familiar with the concept of a scheduler or experienced users who just
    e.g. want to quickly try some library implemented in Picos.  Thanks to the
    defaults, libraries implemented in Picos will e.g. work in an OCaml REPL
    without having to [#require "some_scheduler"] and explicitly run code under
    the scheduler.

    Most serious multicore applications should, however, use some scheduler,
    because a proper scheduler implementation can provide fibers much more
    efficiently.  The defaults are neither intended nor designed to compete with
    actual scheduler implementations.

    {4 Defaults architecture}

    The underlying idea behind the defaults is to make it so that a fiber
    corresponds to a thread (or domain, in case domains are available, but
    threads are not).

    Briefly:

    - The default {{!Fiber.spawn} [spawn]} creates a thread for each fiber.
    - The default {{!Fiber.current} [current]} uses {!TLS} to store the current
      fiber.
    - The default {{!Fiber.yield} [yield]} just calls [Thread.yield].
    - The default {{!Trigger.await} [await]} uses {!TLS} to store a [Mutex] and
      [Condition] to suspend the thread.
    - The default {{!Computation.cancel_after} [cancel_after]} uses {!DLS} to
      store a priority queue of timeouts and a per-domain background timeout
      thread that runs a [Unix.select] loop to cancel computations.

    The default behaviors initialize their resources, per-thread, and per-domain
    state and the background timeout thread, only when actually used.  If the
    default {{!Computation.cancel_after} [cancel_after]} is not used, no
    background timeout thread will be created and no per-domain state will be
    used.  If none of the defaults are used, no per-thread state will be used.

    {4 Partial schedulers}

    The default behaviors are further implemented such that a scheduler
    implementation need not necessarily handle all of the effects.

    In particular, the default {{!Computation.cancel_after} [cancel_after]}
    should basically work fine on any platform where OCaml implements the full
    [Thread] and [Unix] modules.  Notably the default
    {{!Computation.cancel_after} [cancel_after]} will not work on
    {{:https://ocsigen.org/js_of_ocaml/latest/manual/overview} [Js_of_ocaml]}.
    On platforms where the default is available, a scheduler may choose to leave
    the {{!Computation.Cancel_after} [Cancel_after]} effect unhandled.  This can
    be particularly convenient when creating a new scheduler, because the
    {{!Computation.Cancel_after} [Cancel_after]} effect is perhaps the most
    difficult to implement properly.

    The effects that a scheduler should always handle are {{!Fiber.Current}
    [Current]} and {{!Trigger.Await} [Await]}.  Essentially all of the default
    behaviors will use {{!Fiber.current} [current]} to obtain the fiber handle
    and check for cancelation.  So, providing the fiber identity is essential.
    The {{!Trigger.Await} [await]} operation is the most important operation
    used by practically anything and everything implemented in Picos.

    When the {{!Fiber.Spawn} [Spawn]} effect is not handled, the default
    {{!Fiber.spawn} [spawn]} creates new threads and, because a new thread does
    not have any handlers, such threads will use the defaults.  This means that
    it is technically not an issue to leave {{!Fiber.Spawn} [Spawn]} unhandled.
    Typical communication and synchronization abstractions implemented in Picos
    do not spawn fibers and will work fine even when {{!Fiber.Spawn} [Spawn]} is
    not handled.

    The default {{!Fiber.yield} [yield]} behavior of calling [Thread.yield] is
    only useful when threads are being used.  However, practical concurrent
    abstractions implemented in Picos should generally avoid {{!Fiber.yield}
    [yield]}, because it is merely a request to reschedule.  It is typically
    better to {{!Trigger.await} [await]} for something to happen or use
    {{!Computation.cancel_after} [cancel_after]} to sleep in case progress
    cannot be made immediately. *)
