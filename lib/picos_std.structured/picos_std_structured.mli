(** Basic structured concurrency primitives for {!Picos}.

    This library essentially provides one application programming interface for
    structuring fibers with any Picos compatible scheduler.

    For the {{!examples} examples} we open some modules:

    {[
      open Picos_io
      open Picos_std_event
      open Picos_std_finally
      open Picos_std_structured
      open Picos_std_sync
    ]} *)

open Picos_std_event

(** {1 Modules} *)

module Control : sig
  (** Basic control operations and exceptions for structured concurrency. *)

  exception Terminate
  (** An exception that is used to signal fibers, typically by canceling them,
      that they should terminate by letting the exception propagate.

      ℹ️ Within {{!Picos_std_structured} this library}, the [Terminate] exception
      does not, by itself, indicate an error. Raising it inside a fiber forked
      within the structured concurrency constructs of this library simply causes
      the relevant part of the tree of fibers to be terminated.

      ⚠️ If [Terminate] is raised in the main fiber of a {!Bundle}, and no other
      exceptions are raised within any fiber inside the bundle, the bundle will
      then, of course, raise the [Terminate] exception after all of the fibers
      have been terminated. *)

  exception Errors of (exn * Printexc.raw_backtrace) list
  (** An exception that can be used to collect exceptions, typically indicating
      errors, from multiple fibers.

      ℹ️ The {!Terminate} exception is not considered an error within
      {{!Picos_std_structured} this library} and the structuring constructs do
      not include it in the list of [Errors]. *)

  val raise_if_canceled : unit -> unit
  (** [raise_if_canceled ()] checks whether the current fiber has been canceled
      and if so raises the exception that the fiber was canceled with.

      ℹ️ Within {{!Picos_std_structured} this library} fibers are canceled using
      the {!Terminate} exception. *)

  val yield : unit -> unit
  (** [yield ()] asks the current fiber to be rescheduled. *)

  val sleep : seconds:float -> unit
  (** [sleep ~seconds] suspends the current fiber for the specified number of
      [seconds]. *)

  val protect : (unit -> 'a) -> 'a
  (** [protect thunk] forbids propagation of cancelation for the duration of
      [thunk ()].

      ℹ️ {{!Picos_std_sync} Many operations are cancelable}. In particular,
      anything that might suspend the current fiber to await for something
      should typically be cancelable. Operations that release resources may
      sometimes also be cancelable and
      {{!Picos_std_finally.finally} calls of such operations should typically be
       protected} to ensure that resources will be properly released. Forbidding
      propagation of cancelation may also be required when a sequence of
      cancelable operations must be performed.

      ℹ️ With the constructs provided by {{!Picos_std_structured} this library}
      it is not possible to prevent a fiber from being canceled, but it is
      possible for a fiber to forbid the scheduler from propagating cancelation
      to the fiber. *)

  val block : unit -> 'a
  (** [block ()] suspends the current fiber until it is canceled at which point
      the cancelation exception will be raised.

      @raise Invalid_argument
        in case propagation of cancelation has been {{!protect} forbidden}.

      @raise Sys_error
        in case the underlying computation of the fiber is forced to return
        during [block]. This is only possible when the fiber has been spawned
        through another library. *)

  val terminate_after : ?callstack:int -> seconds:float -> (unit -> 'a) -> 'a
  (** [terminate_after ~seconds thunk] arranges to terminate the execution of
      [thunk] on the current fiber after the specified timeout in [seconds].

      Using [terminate_after] one can attempt any blocking operation that
      supports cancelation with a timeout. For example, one could try to
      {{!Picos_std_sync.Ivar.read} [read]} an {{!Picos_std_sync.Ivar} [Ivar]}
      with a timeout

      {[
        let peek_in ~seconds ivar =
          match
            Control.terminate_after ~seconds @@ fun () -> Ivar.read ivar
          with
          | value -> Some value
          | exception Control.Terminate -> None
      ]}

      or one could try to {{!Picos_io.Unix.connect} [connect]} a socket with a
      timeout

      {[
        let try_connect_in ~seconds socket sockaddr =
          match
            Control.terminate_after ~seconds @@ fun () ->
            Unix.connect socket sockaddr
          with
          | () -> true
          | exception Control.Terminate -> false
      ]}

      using the {!Picos_io.Unix} module.

      The optional [callstack] argument specifies the number of callstack
      entries to capture with the {{!Control.Terminate} [Terminate]} exception.
      The default is [0].

      As an example, [terminate_after] could be implemented using {!Bundle} as
      follows:
      {[
        let terminate_after ?callstack ~seconds thunk =
          Bundle.join_after @@ fun bundle ->
          Bundle.terminate_after ?callstack ~seconds bundle;
          thunk ()
      ]} *)
end

module Promise : sig
  (** A cancelable promise.

      ℹ️ In addition to using a promise to concurrently compute and return a
      value, a cancelable promise can also represent a concurrent fiber that
      will continue until it is explicitly {{!try_terminate} canceled}.

      ⚠️ {{!try_terminate} Canceling} a promise does not immediately terminate
      the fiber or wait for the fiber working to complete the promise to
      terminate. On the other hand, constructs like {!Bundle.join_after} and
      {!Flock.join_after} guarantee that all the fibers forked within their
      scope have actually terminated before they return or raise. The reason for
      this design choice in this library is that synchronization is expensive
      and delaying synchronization to the join operation is typically sufficient
      and amortizes the cost. *)

  type !'a t
  (** Represents a promise to produce a value of type ['a]. *)

  val of_value : 'a -> 'a t
  (** [of_value value] returns a constant completed promise that returns the
      given [value].

      ℹ️ Promises can also be created in the scope of a
      {{!Bundle.fork_as_promise} [Bundle]} or a
      {{!Flock.fork_as_promise} [Flock]}. *)

  val await : 'a t -> 'a
  (** [await promise] awaits until the promise has completed and either returns
      the [value] that the evaluation of the promise returned, raises the
      exception that the evaluation of the promise raised, or raises the
      {{!Control.Terminate} [Terminate]} exception in case the promise has been
      canceled.

      ⚠️ The fiber corresponding to a {{!try_terminate} canceled} promise is not
      guaranteed to have terminated at the point [await] raises. *)

  val completed : 'a t -> 'a Event.t
  (** [completed promise] returns an {{!Picos_std_event.Event} event} that can
      be committed to once the promise has completed. *)

  val is_running : 'a t -> bool
  (** [is_running promise] determines whether the completion of the promise is
      still pending. *)

  val try_terminate : ?callstack:int -> 'a t -> bool
  (** [try_terminate promise] tries to terminate the promise by canceling it
      with the {{!Control.Terminate} [Terminate]} exception and returns [true]
      in case of success and [false] in case the promise had already completed,
      i.e. either returned, raised, or canceled.

      The optional [callstack] argument specifies the number of callstack
      entries to capture with the {{!Control.Terminate} [Terminate]} exception.
      The default is [0]. *)

  val terminate : ?callstack:int -> 'a t -> unit
  (** [terminate promise] is equivalent to
      {{!try_terminate} [try_terminate promise |> ignore]}. *)

  val terminate_after : ?callstack:int -> 'a t -> seconds:float -> unit
  (** [terminate_after ~seconds promise] arranges to terminate the [promise] by
      canceling it with the {{!Control.Terminate} [Terminate]} exception after
      the specified timeout in [seconds].

      The optional [callstack] argument specifies the number of callstack
      entries to capture with the {{!Control.Terminate} [Terminate]} exception.
      The default is [0]. *)
end

module Bundle : sig
  (** An explicit dynamic bundle of fibers guaranteed to be joined at the end.

      Bundles allow you to conveniently structure or delimit concurrency into
      nested scopes. After a bundle returns or raises an exception, no fibers
      {{!fork} forked} to the bundle remain.

      An unhandled exception, or error, within any fiber of the bundle causes
      all of the fibers {{!fork} forked} to the bundle to be canceled and the
      bundle to raise the error exception or
      {{!Control.Errors} error exceptions} raised by all of the fibers forked
      into the bundle. *)

  type t
  (** Represents a bundle of fibers. *)

  val join_after :
    ?callstack:int -> ?on_return:[ `Terminate | `Wait ] -> (t -> 'a) -> 'a
  (** [join_after scope] calls [scope] with a {{!t} bundle}. A call of
      [join_after] returns or raises only after [scope] has returned or raised
      and all {{!fork} forked} fibers have terminated. If [scope] raises an
      exception, {!error} will be called.

      The optional [on_return] argument specifies what to do when the scope
      returns normally. It defaults to [`Wait], which means to just wait for all
      the fibers to terminate on their own. When explicitly specified as
      [~on_return:`Terminate], then {{!terminate} [terminate ?callstack]} will
      be called on return. This can be convenient, for example, when dealing
      with {{:https://en.wikipedia.org/wiki/Daemon_(computing)} daemon} fibers.
  *)

  val terminate : ?callstack:int -> t -> unit
  (** [terminate bundle] cancels all of the {{!fork} forked} fibers using the
      {{!Control.Terminate} [Terminate]} exception. After [terminate] has been
      called, no new fibers can be forked to the bundle.

      The optional [callstack] argument specifies the number of callstack
      entries to capture with the {{!Control.Terminate} [Terminate]} exception.
      The default is [0].

      ℹ️ Calling [terminate] at the end of a bundle can be a convenient way to
      cancel any background fibers started by the bundle.

      ℹ️ Calling [terminate] does not raise the
      {{!Control.Terminate} [Terminate]} exception, but blocking operations
      after [terminate] will raise the exception to propagate cancelation unless
      {{!Control.protect} propagation of cancelation is forbidden}. *)

  val terminate_after : ?callstack:int -> t -> seconds:float -> unit
  (** [terminate_after ~seconds bundle] arranges to {!terminate} the bundle
      after the specified timeout in [seconds]. *)

  val error : ?callstack:int -> t -> exn -> Printexc.raw_backtrace -> unit
  (** [error bundle exn bt] first calls {!terminate} and then adds the exception
      with backtrace to the list of exceptions to be raised, unless the
      exception is the {{!Control.Terminate} [Terminate]} exception, which is
      not considered to signal an error by itself.

      The optional [callstack] argument is passed to {!terminate}. *)

  val fork_as_promise : t -> (unit -> 'a) -> 'a Promise.t
  (** [fork_as_promise bundle thunk] spawns a new fiber to the [bundle] that
      will run the given [thunk]. The result of the [thunk] will be written to
      the {{!Promise} promise}. If the [thunk] raises an exception, {!error}
      will be called with that exception. *)

  val fork : t -> (unit -> unit) -> unit
  (** [fork bundle action] is equivalent to
      {{!fork_as_promise} [fork_as_promise bundle action |> ignore]}. *)
end

module Flock : sig
  (** An implicit dynamic flock of fibers guaranteed to be joined at the end.

      Flocks allow you to conveniently structure or delimit concurrency into
      nested scopes. After a flock returns or raises an exception, no fibers
      {{!fork} forked} to the flock remain.

      An unhandled exception, or error, within any fiber of the flock causes all
      of the fibers {{!fork} forked} to the flock to be canceled and the flock
      to raise the error exception or {{!Control.Errors} error exceptions}
      raised by all of the fibers forked into the flock.

      ℹ️ This is essentially a very thin convenience wrapper for an implicitly
      propagated {!Bundle}.

      ⚠️ All of the operations in this module, except {!join_after}, raise the
      {!Invalid_argument} exception in case they are called from outside of the
      dynamic multifiber scope of a flock established by calling {!join_after}.
  *)

  val join_after :
    ?callstack:int -> ?on_return:[ `Terminate | `Wait ] -> (unit -> 'a) -> 'a
  (** [join_after scope] creates a new flock for fibers, calls [scope] after
      setting current flock to the new flock, and restores the previous flock,
      if any after [scope] exits. The flock will be implicitly propagated to all
      fibers {{!fork} forked} into the flock. A call of [join_after] returns or
      raises only after [scope] has returned or raised and all {{!fork} forked}
      fibers have terminated. If [scope] raises an exception, {!error} will be
      called.

      The optional [on_return] argument specifies what to do when the scope
      returns normally. It defaults to [`Wait], which means to just wait for all
      the fibers to terminate on their own. When explicitly specified as
      [~on_return:`Terminate], then {{!terminate} [terminate ?callstack]} will
      be called on return. This can be convenient, for example, when dealing
      with {{:https://en.wikipedia.org/wiki/Daemon_(computing)} daemon} fibers.
  *)

  val terminate : ?callstack:int -> unit -> unit
  (** [terminate ()] cancels all of the {{!fork} forked} fibers using the
      {{!Control.Terminate} [Terminate]} exception. After [terminate] has been
      called, no new fibers can be forked to the current flock.

      The optional [callstack] argument specifies the number of callstack
      entries to capture with the {{!Control.Terminate} [Terminate]} exception.
      The default is [0].

      ℹ️ Calling [terminate] at the end of a flock can be a convenient way to
      cancel any background fibers started by the flock.

      ℹ️ Calling [terminate] does not raise the
      {{!Control.Terminate} [Terminate]} exception, but blocking operations
      after [terminate] will raise the exception to propagate cancelation unless
      {{!Control.protect} propagation of cancelation is forbidden}. *)

  val terminate_after : ?callstack:int -> seconds:float -> unit -> unit
  (** [terminate_after ~seconds ()] arranges to {!terminate} the current flock
      after the specified timeout in [seconds]. *)

  val error : ?callstack:int -> exn -> Printexc.raw_backtrace -> unit
  (** [error exn bt] first calls {!terminate} and then adds the exception with
      backtrace to the list of exceptions to be raised, unless the exception is
      the {{!Control.Terminate} [Terminate]} exception, which is not considered
      to signal an error by itself.

      The optional [callstack] argument is passed to {!terminate}. *)

  val fork_as_promise : (unit -> 'a) -> 'a Promise.t
  (** [fork_as_promise thunk] spawns a new fiber to the current flock that will
      run the given [thunk]. The result of the [thunk] will be written to the
      {{!Promise} promise}. If the [thunk] raises an exception, {!error} will be
      called with that exception. *)

  val fork : (unit -> unit) -> unit
  (** [fork action] is equivalent to
      {{!fork_as_promise} [fork_as_promise action |> ignore]}. *)
end

module Run : sig
  (** Operations for running fibers in specific patterns. *)

  val all : (unit -> unit) list -> unit
  (** [all actions] starts the actions as separate fibers and waits until they
      all complete or one of them raises an unhandled exception other than
      {{!Control.Terminate} [Terminate]}, which is not counted as an error,
      after which the remaining fibers will be canceled.

      ⚠️ One of the actions may be run on the current fiber.

      ⚠️ It is not guaranteed that any of the actions in the list are called. In
      particular, after any action raises an unhandled exception or after the
      main fiber is canceled, the actions that have not yet started may be
      skipped entirely.

      [all] is roughly equivalent to
      {[
        let all actions =
          Bundle.join_after @@ fun bundle ->
          List.iter (Bundle.fork bundle) actions
      ]}
      but treats the list of actions as a single computation. *)

  val any : (unit -> unit) list -> unit
  (** [any actions] starts the actions as separate fibers and waits until one of
      them completes or raises an unhandled exception other than
      {{!Control.Terminate} [Terminate]}, which is not counted as an error,
      after which the rest of the started fibers will be canceled.

      ⚠️ One of the actions may be run on the current fiber.

      ⚠️ It is not guaranteed that any of the actions in the list are called. In
      particular, after the first action returns successfully or after any
      action raises an unhandled exception or after the main fiber is canceled,
      the actions that have not yet started may be skipped entirely.

      [any] is roughly equivalent to
      {[
        let any actions =
          Bundle.join_after @@ fun bundle ->
          try
            actions
            |> List.iter @@ fun action ->
               Bundle.fork bundle @@ fun () ->
               action ();
               Bundle.terminate bundle
          with Control.Terminate -> ()
      ]}
      but treats the list of actions as a single computation. *)
end

(** {1 Examples}

    {2 Understanding cancelation}

    Consider the following program:

    {[
      let main () =
        Flock.join_after @@ fun () ->
        let promise = Flock.fork_as_promise @@ fun () -> Control.block () in

        begin
          Flock.fork @@ fun () -> Promise.await promise
        end;

        begin
          Flock.fork @@ fun () ->
          let condition = Condition.create () and mutex = Mutex.create () in
          Mutex.protect mutex @@ fun () ->
          while true do
            Condition.wait condition mutex
          done
        end;

        begin
          Flock.fork @@ fun () ->
          let sem = Semaphore.Binary.make false in
          Semaphore.Binary.acquire sem
        end;

        begin
          Flock.fork @@ fun () ->
          let sem = Semaphore.Counting.make 0 in
          Semaphore.Counting.acquire sem
        end;

        begin
          Flock.fork @@ fun () -> Event.sync (Event.choose [])
        end;

        begin
          Flock.fork @@ fun () ->
          let latch = Latch.create 1 in
          Latch.await latch
        end;

        begin
          Flock.fork @@ fun () ->
          let ivar = Ivar.create () in
          Ivar.read ivar
        end;

        begin
          Flock.fork @@ fun () ->
          let stream = Stream.create () in
          Stream.read (Stream.tap stream) |> ignore
        end;

        begin
          Flock.fork @@ fun () ->
          let@ inn, out =
            finally Unix.close_pair @@ fun () ->
            Unix.socketpair ~cloexec:true PF_UNIX SOCK_STREAM 0
          in
          Unix.set_nonblock inn;
          let n = Unix.read inn (Bytes.create 1) 0 1 in
          assert (n = 1)
        end;

        begin
          Flock.fork @@ fun () ->
          let a_month = 60.0 *. 60.0 *. 24.0 *. 30.0 in
          Control.sleep ~seconds:a_month
        end;

        (* Let the children get stuck *)
        Control.sleep ~seconds:0.1;

        Flock.terminate ()
    ]}

    First of all, note that above the {{!Picos_std_sync.Mutex} [Mutex]},
    {{!Picos_std_sync.Condition} [Condition]}, and
    {{!Picos_std_sync.Semaphore} [Semaphore]} modules come from the
    {!Picos_std_sync} library and the {{!Picos_io.Unix} [Unix]} module comes
    from the {!Picos_io} library. They do not come from the standard OCaml
    libraries.

    The above program creates a {{!Flock} flock} of fibers and
    {{!Flock.fork} forks} several fibers to the flock that all block in various
    ways. In detail,

    - {!Control.block} never returns,
    - {!Promise.await} never returns as the promise won't be completed,
    - {{!Picos_std_sync.Condition.wait} [Condition.wait]} never returns, because
      the condition is never signaled,
    - {{!Picos_std_sync.Semaphore.Binary.acquire} [Semaphore.Binary.acquire]}
      and
      {{!Picos_std_sync.Semaphore.Counting.acquire}
       [Semaphore.Counting.acquire]} never return, because the counts of the
      semaphores never change from [0],
    - {{!Picos_std_event.Event.sync} [Event.sync]} never returns, because the
      event can never be committed to,
    - {{!Picos_std_sync.Latch.await} [Latch.await]} never returns, because the
      count of the latch never reaches [0],
    - {{!Picos_std_sync.Ivar.read} [Ivar.read]} never returns, because the
      incremental variable is never filled,
    - {{!Picos_std_sync.Stream.read} [Stream.read]} never returns, because the
      stream is never pushed to,
    - {{!Picos_io.Unix.read} [Unix.read]} never returns, because the socket is
      never written to, and the
    - {!Control.sleep} call would return only after about a month.

    Fibers forked to a flock can be canceled in various ways. In the above
    program we call {!Flock.terminate} to cancel all of the fibers and
    effectively close the flock. This allows the program to return normally
    immediately and without leaking or leaving anything in an invalid state:

    {[
      # Picos_mux_random.run_on ~n_domains:2 main
      - : unit = ()
    ]}

    Now, the point of the above example isn't that you should just call
    {{!Flock.terminate} [terminate]} when your program gets stuck. 😅

    What the above example hopefully demonstrates is that concurrent
    abstractions like mutexes and condition variables, asynchronous IO
    libraries, and others can be designed to support cancelation.

    Cancelation is a signaling mechanism that allows structured concurrent
    abstractions, like the {!Flock} abstraction, to (hopefully) gracefully tear
    down concurrent fibers in case of errors. Indeed, one of the basic ideas
    behind the {!Flock} abstraction is that in case any fiber forked to the
    flock raises an unhandled exception, the whole flock will be terminated and
    the error will be raised from the flock, which allows you to understand what
    went wrong, instead of having to debug a program that mysteriously gets
    stuck, for example.

    Cancelation can also, with some care, be used as a mechanism to terminate
    fibers once they are no longer needed. However, just like sleep, for
    example, cancelation is inherently prone to races, i.e. it is difficult to
    understand the exact point and state at which a fiber gets canceled and it
    is usually non-deterministic, and therefore cancelation is not recommended
    for use as a general synchronization or communication mechanism.

    {2 Errors and cancelation}

    Consider the following program:

    {[
      let many_errors () =
        Flock.join_after @@ fun () ->
        let latch = Latch.create 1 in

        let fork_raising exn =
          Flock.fork @@ fun () ->
          begin
            Control.protect @@ fun () -> Latch.await latch
          end;
          raise exn
        in

        fork_raising Exit;
        fork_raising Not_found;
        fork_raising Control.Terminate;

        Latch.decr latch
    ]}

    The above program starts three fibers and uses a
    {{!Picos_std_sync.Latch} latch} to ensure that all of them have been
    started, before two of them raise errors and the third raises
    {{!Control.Terminate} [Terminate]}, which is not considered an error in this
    library. Running the program

    {[
      # Picos_mux_fifo.run many_errors
      Exception: Errors[Stdlib.Exit; Not_found]
    ]}

    raises a collection of all of the {{!Control.Errors} errors}.

    {2 A simple echo server and clients}

    Let's build a simple TCP echo server and run it with some clients.

    We first define a function for the server:

    {[
      let run_server server_fd =
        Flock.join_after @@ fun () ->
        while true do
          let@ client_fd =
            instantiate Unix.close @@ fun () ->
            Unix.accept ~cloexec:true server_fd |> fst
          in

          (* Fork a fiber for client *)
          Flock.fork @@ fun () ->
          let@ client_fd = move client_fd in
          Unix.set_nonblock client_fd;

          let bs = Bytes.create 100 in
          let n = Unix.read client_fd bs 0 (Bytes.length bs) in
          Unix.write client_fd bs 0 n |> ignore
        done
    ]}

    The server function expects a listening socket. For each accepted client the
    server forks a new fiber to handle it. The client socket is
    {{!Finally.move} moved} from the server fiber to the client fiber to avoid
    leaks and to ensure that the socket will be closed.

    Let's then define a function for the clients:

    {[
      let run_client server_addr =
        let@ socket =
          finally Unix.close @@ fun () ->
          Unix.socket ~cloexec:true PF_INET SOCK_STREAM 0
        in
        Unix.set_nonblock socket;
        Unix.connect socket server_addr;

        let msg = "Hello!" in
        Unix.write_substring socket msg 0 (String.length msg) |> ignore;

        let bytes = Bytes.create (String.length msg) in
        let n = Unix.read socket bytes 0 (Bytes.length bytes) in

        Printf.printf "Received: %s\n%!" (Bytes.sub_string bytes 0 n)
    ]}

    The client function takes the address of the server and connects a socket to
    the server address. It then writes a message to the server and reads a reply
    from the server and prints it.

    Here is the main program:

    {[
      let main () =
        let@ server_fd =
          finally Unix.close @@ fun () ->
          Unix.socket ~cloexec:true PF_INET SOCK_STREAM 0
        in
        Unix.set_nonblock server_fd;
        (* Let system determine the port *)
        Unix.bind server_fd Unix.(ADDR_INET (inet_addr_loopback, 0));
        Unix.listen server_fd 8;

        let server_addr = Unix.getsockname server_fd in

        Flock.join_after ~on_return:`Terminate @@ fun () ->
        (* Start server *)
        begin
          Flock.fork @@ fun () -> run_server server_fd
        end;

        (* Run clients concurrently *)
        Flock.join_after @@ fun () ->
        for _ = 1 to 5 do
          Flock.fork @@ fun () -> run_client server_addr
        done
    ]}

    The main program creates a socket for the server and configures it. The
    server is then started as a fiber in a flock terminated on return. Then the
    clients are started to run concurrently in an inner flock.

    Finally we run the main program with a scheduler:

    {[
      # Picos_mux_random.run_on ~n_domains:1 main
      Received: Hello!
      Received: Hello!
      Received: Hello!
      Received: Hello!
      Received: Hello!
      - : unit = ()
    ]}

    As an exercise, you might want to refactor the server to avoid
    {{!Finally.move} moving} the file descriptors and use a
    {{!Finally.let@} recursive} accept loop instead. You could also
    {{!Flock.terminate} terminate the whole flock} at the end instead of just
    terminating the server. *)
