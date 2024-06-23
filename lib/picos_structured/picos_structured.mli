(** Basic structured concurrency primitives for {!Picos}.

    This library essentially provides one application programming interface for
    structuring fibers with any Picos compatible scheduler.

    For the {{!examples} examples} we open some modules:

    {[
      open Picos_structured.Finally
      open Picos_structured
      open Picos_stdio
      open Picos_sync
    ]} *)

open Picos
open Picos_sync

(** {1 Modules} *)

module Finally : sig
  (** Syntax for avoiding resource leaks. *)

  type 'a finally = ('a -> unit) * (unit -> 'a)
  (** A pair of release and acquire functions. *)

  val finally : ('a -> unit) -> (unit -> 'a) -> 'a finally
  (** [finally release acquire] is equivalent to [(release, acquire)]. *)

  val ( let@ ) : 'a finally -> ('a -> 'b) -> 'b
  (** [let@ resource = finally release acquire in scope] calls [acquire ()] to
      obtain a [resource], evaluates [scope], and calls [release resource]
      whether [scope] returns normally or raises an exception.

      Here is a sketch of a server that recursively forks a fiber to accept and
      handle a client:

      {@ocaml skip[
        Bundle.join_after @@ fun bundle ->
        let rec accept () =
          let@ client_fd =
            finally Unix.close @@ fun () ->
            Unix.accept ~cloexec:true server_fd
            |> fst
          in
          (* fork to accept other clients *)
          Bundle.fork bundle accept;
          (* handle this client... omitted *)
        in
        Bundle.fork bundle accept
      ]}

      There is also a way to {!move} resources to allow forking fibers to handle
      clients without leaks. *)

  type 'a moveable
  (** A [moveable] either contains a resource or is empty as the resource has
      been moved. *)

  val ( let^ ) : 'a finally -> ('a moveable -> 'b) -> 'b
  (** [let^ moveable = finally release acquire in scope] calls [acquire ()] to
      obtain a resource and stores it as a [moveable] resource.  Then, at the
      end of [scope], awaits that the resource is {{!move} moved} out of the
      [moveable] or [release]s the resource in case of cancelation. *)

  val move : 'a moveable -> 'a finally
  (** [move moveable] creates {{!type-finally} a pair of release and acquire
      functions} where the acquire operation takes the resource from the
      [moveable] and the release operation releases the resource.

      Here is a sketch of a server that accepts in a loop and forks fibers to
      handle clients:

      {@ocaml skip[
        Bundle.join_after @@ fun bundle ->
        while true do
          (* loop to accept clients *)
          let^ client_fd =
            finally Unix.close @@ fun () ->
            Unix.accept ~closexec:true server_fd
            |> fst
          in
          (* fork to handle this client *)
          Bundle.fork bundle @@ fun () ->
            let@ client_fd = move client_fd in
            (* handle client... omitted *)
        done
      ]}

      Another alternative to avoiding leaks is to {{!let@} recursively fork
      fibers to accept and handle a client}.

      @raise Invalid_argument if the resource has already been moved (or
        released) unless the fiber has been canceled. *)
end

module Control : sig
  (** Basic control operations and exceptions for structured concurrency. *)

  exception Terminate
  (** An exception that is used to signal fibers, typically by canceling them,
      that they should terminate by letting the exception propagate.

      ℹ️ Within {{!Picos_structured} this library}, the [Terminate] exception
      does not, by itself, indicate an error.  Raising it inside a fiber forked
      within the structured concurrency constructs of this library simply causes
      the relevant part of the tree of fibers to be terminated.

      ⚠️ If [Terminate] is raised in the main fiber of a {!Bundle}, and no other
      exceptions are raised within any fiber inside the bundle, the bundle will
      then, of course, raise the [Terminate] exception after all the fibers have
      been terminated. *)

  exception Errors of Exn_bt.t list
  (** An exception that can be used to collect exceptions, typically indicating
      errors, from multiple fibers.

      ℹ️ The {!Terminate} exception is not considered an error within
      {{!Picos_structured} this library} and the structuring constructs do not
      include it in the list of [Errors]. *)

  val raise_if_canceled : unit -> unit
  (** [raise_if_canceled ()] checks whether the current fiber has been canceled
      and if so raises the exception that the fiber was canceled with.

      ℹ️ Within {{!Picos_structured} this library} fibers are canceled using the
      {!Terminate} exception. *)

  val yield : unit -> unit
  (** [yield ()] asks the current fiber to be rescheduled. *)

  val sleep : seconds:float -> unit
  (** [sleep ~seconds] suspends the current fiber for the specified number of
      [seconds]. *)

  val protect : (unit -> 'a) -> 'a
  (** [protect thunk] forbids propagation of cancelation for the duration of
      [thunk ()].

      ℹ️ With the constructs provided by {{!Picos_structured} this library} it is
      not possible to prevent a fiber from being canceled, but it is possible
      for a fiber to forbid the scheduler from propagating cancelation to the
      fiber. *)

  val block : unit -> 'a
  (** [block ()] suspends the current fiber until it is canceled at which point
      the cancelation exception will be raised.

      @raise Invalid_argument in case propagation of cancelation has been
        {{!protect} forbidden}. *)
end

module Promise : sig
  (** A cancelable promise.

      ℹ️ In addition to using a promise to concurrently compute and return a
      value, a cancelable promise can also represent a concurrent fiber that
      will continue until it is explicitly {{!try_terminate} canceled}. *)

  type !'a t
  (** Represents a promise to return value of type ['a]. *)

  val of_value : 'a -> 'a t
  (** [of_value value] returns a constant completed promise that returns the
      given [value].

      ℹ️ Promises can also be {{!Bundle.fork_as_promise} created} in the scope of
      a {!Bundle}. *)

  val await : 'a t -> 'a
  (** [await promise] awaits until the promise has completed and either returns
      the [value] that the evaluation of the promise returned, raises the
      exception that the evaluation of the promise raised, or raises the
      {{!Control.Terminate} [Terminate]} exception in case the promise has been
      canceled. *)

  val completed : 'a t -> 'a Event.t
  (** [completed promise] returns an {{!Picos_sync.Event} event} that can be
      committed to once the promise has completed. *)

  val is_running : 'a t -> bool
  (** [is_running promise] determines whether the completion of the promise is
      still pending. *)

  val try_terminate : ?callstack:int -> 'a t -> bool
  (** [try_terminate promise] tries to terminate the promise by canceling it
      with the {{!Control.Terminate} [Terminate]} exception and returns [true]
      in case of success and [false] in case the promise had already completed,
      i.e. either returned, raised, or canceled. *)

  val terminate : ?callstack:int -> 'a t -> unit
  (** [terminate promise] is equivalent to
      {{!try_terminate} [try_terminate promise |> ignore]}. *)
end

module Bundle : sig
  (** A dynamic bundle of fibers guaranteed to be joined at the end.

      Bundles allow you to conveniently structure or delimit concurrency into
      nested scopes.  After a bundle returns or raises an exception, no fibers
      {{!fork} forked} to the bundle remain.

      An unhandled exception, or error, within any fiber of the bundle causes
      all the fibers {{!fork} forked} to the bundle to be canceled and the
      bundle to raise the exception. *)

  type t
  (** Represents a bundle of fibers. *)

  val join_after : (t -> 'a) -> 'a
  (** [join_after scope] calls [scope] with a {{!t} bundle}.  A call of
      [join_after] returns or raises only after [scope] has returned or raised
      and all {{!fork} forked} fibers have terminated.  If [scope] raises an
      exception, {!error} will be called.

      ℹ️ When [scope] returns normally, {!terminate} will not be called
      implicitly. *)

  val terminate : ?callstack:int -> t -> unit
  (** [terminate bundle] cancels all the {{!fork} forked} fibers using the
      {{!Control.Terminate} [Terminate]} exception.  After [terminate] has been
      called, no new fibers can be forked to the bundle.

      The optional [callstack] argument specifies the number of callstack
      entries to capture with the {{!Control.Terminate} [Terminate]} exception.
      The default is [0].

      ℹ️ Calling [terminate] at the end of a bundle can be a convenient way to
      cancel any background fibers started by the bundle.

      ℹ️ Calling [terminate] does not raise the {{!Control.Terminate}
      [Terminate]} exception, but blocking operations after [terminate] will
      raise the exception to propagate cancelation unless {{!Control.protect}
      propagation of cancelation is forbidden}. *)

  val error : ?callstack:int -> t -> Exn_bt.t -> unit
  (** [error bundle exn_bt] first calls {!terminate} and then adds the exception
      with backtrace to the list of exceptions to be raised, unless the
      exception is the {{!Control.Terminate} [Terminate]} exception, which is
      not considered to signal an error by itself.

      The optional [callstack] argument is passed to {!terminate}. *)

  val fork_as_promise : t -> (unit -> 'a) -> 'a Promise.t
  (** [fork_as_promise bundle thunk] spawns a new fiber to the [bundle] that
      will run the given [thunk].  The result of the [thunk] will be written to
      the {{!Promise} promise}.  If the [thunk] raises an exception, {!error}
      will be called with that exception. *)

  val fork : t -> (unit -> unit) -> unit
  (** [fork bundle action] is equivalent to
      {{!fork_as_promise} [fork_as_promise bundle action |> ignore]}. *)
end

module Run : sig
  (** Operations for running fibers in specific patterns. *)

  val all : (unit -> unit) list -> unit
  (** [all actions] starts the actions as separate fibers and waits until they
      all complete or one of them raises an unhandled exception other than
      {{!Control.Terminate} [Terminate]}, which is not counted as an error,
      after which the remaining fibers will be canceled.

      ⚠️ It is not guaranteed that any of the actions in the list are called.  In
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

      ⚠️ It is not guaranteed that any of the actions in the list are called.  In
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
        Bundle.join_after begin fun bundle ->
          let promise =
            Bundle.fork_as_promise bundle
            @@ fun () -> Control.block ()
          in

          Bundle.fork bundle begin fun () ->
            Promise.await promise
          end;

          Bundle.fork bundle begin fun () ->
            let condition = Condition.create ()
            and mutex = Mutex.create () in
            Mutex.protect mutex begin fun () ->
              while true do
                Condition.wait condition mutex
              done
            end
          end;

          Bundle.fork bundle begin fun () ->
            let@ inn, out = finally
              Unix.close_pair @@ fun () ->
              Unix.socketpair ~cloexec:true
                PF_UNIX SOCK_STREAM 0
            in
            Unix.set_nonblock inn;
            let n =
              Unix.read inn (Bytes.create 1)
                0 1
            in
            assert (n = 1)
          end;

          Bundle.fork bundle begin fun () ->
            let a_month =
              60.0 *. 60.0 *. 24.0 *. 30.0
            in
            Control.sleep ~seconds:a_month
          end;

          (* Let the children get stuck *)
          Control.yield ();

          Bundle.terminate bundle
        end
    ]}

    First of all, note that above the {{!Picos_sync.Mutex} [Mutex]} and
    {{!Picos_sync.Condition} [Condition]} modules come from the {!Picos_sync}
    library and the {{!Picos_stdio.Unix} [Unix]} module comes from the
    {!Picos_stdio} library.  They do not come from the standard OCaml libraries.

    The above program creates a {{!Bundle} bundle} of fibers and {{!Bundle.fork}
    forks} several fibers to the bundle that all block in various ways.  In
    detail,

    - {!Control.block} never returns,
    - {!Promise.await} never returns as the [promise] won't be completed,
    - {{!Picos_sync.Condition.wait} [Condition.wait]} never returns, because the
      condition is never signaled,
    - {{!Picos_stdio.Unix.read} [Unix.read]} never returns, because the socket
      is never written to, and the
    - {!Control.sleep} call would return only after about a month.

    Fibers forked to a bundle can be canceled in various ways.  In the above
    program we call {!Bundle.terminate} to cancel all the fibers and effectively
    close the bundle.  This allows the program to return normally immediately
    and without leaking or leaving anything in an invalid state:

    {[
      # Picos_fifos.run main
      - : unit = ()
    ]}

    Now, the point of the above example isn't that you should just call
    {{!Bundle.terminate} [terminate]} when your program gets stuck. 😅

    What the above example hopefully demonstrates is that concurrent
    abstractions like mutexes and condition variables, asynchronous IO
    libraries, and others can be designed to support cancelation.

    Cancelation is a control flow mechanism that allows structured concurrent
    abstractions, like the {!Bundle} abstraction, to (hopefully) gracefully tear
    down concurrent fibers in case of errors.  Indeed, one of the basic ideas
    behind the {!Bundle} abstraction is that in case any fiber forked to the
    bundle raises an unhandled exception, the whole bundle will be terminated
    and the error will raised from the bundle, which allows you to understand
    what went wrong, instead of having to debug a program that mysteriously gets
    stuck, for example.

    Cancelation can also, with some care, be used as a mechanism to terminate
    fibers once they are no longer needed.  However, just like sleep, for
    example, cancelation is inherently prone to races, i.e. it is difficult to
    understand the exact point and state at which a fiber gets canceled and it
    is usually non-deterministic, and therefore cancelation is not recommended
    for use as a general synchronization or communication mechanism.

    {2 A simple echo server and clients}

    Let's build a simple TCP echo server and run it with some clients.

    We first define a function for the server:

    {[
      let run_server server_fd =
        Unix.listen server_fd 8;

        Bundle.join_after begin fun bundle ->
          while true do
            let^ client_fd =
              finally Unix.close @@ fun () ->
              Unix.accept
                ~cloexec:true server_fd |> fst
            in

            (* Fork a fiber for client *)
            Bundle.fork bundle begin fun () ->
              let@ client_fd =
                move client_fd
              in
              Unix.set_nonblock client_fd;

              let bs = Bytes.create 100 in
              let n =
                Unix.read client_fd bs 0
                  (Bytes.length bs)
              in
              Unix.write client_fd bs 0 n
              |> ignore
            end
          done
        end
    ]}

    The server function expects a bound socket and starts listening.  For each
    accepted client the server forks a new fiber to handle it.  The client
    socket is {{!Finally.move} moved} from the server fiber to the client fiber
    to avoid leaks and to ensure that the socket will be closed.

    Let's then define a function for the clients:

    {[
      let run_client server_addr =
        let@ socket =
          finally Unix.close @@ fun () ->
          Unix.socket ~cloexec:true
            PF_INET SOCK_STREAM 0
        in
        Unix.set_nonblock socket;
        Unix.connect socket server_addr;

        let msg = "Hello!" in
        Unix.write_substring
          socket msg 0 (String.length msg)
        |> ignore;

        let bytes =
          Bytes.create (String.length msg)
        in
        let n =
          Unix.read socket bytes 0
            (Bytes.length bytes)
        in

        Printf.printf "Received: %s\n%!"
          (Bytes.sub_string bytes 0 n)
    ]}

    The client function takes the address of the server and connects a socket to
    the server address.  It then writes a message to the server and reads a
    reply from the server and prints it.

    Here is the main program:

    {[
      let main () =
        let@ server_fd =
          finally Unix.close @@ fun () ->
          Unix.socket ~cloexec:true
            PF_INET SOCK_STREAM 0
        in
        Unix.set_nonblock server_fd;

        (* Let system determine the port *)
        Unix.bind server_fd Unix.(
          ADDR_INET(inet_addr_loopback, 0));

        let server_addr =
          Unix.getsockname server_fd
        in

        Bundle.join_after begin fun bundle ->
          (* Start server *)
          let server =
            Bundle.fork_as_promise bundle
            @@ fun () -> run_server server_fd
          in

          (* Run clients concurrently *)
          Bundle.join_after begin fun bundle ->
            for _ = 1 to 5 do
              Bundle.fork bundle @@ fun () ->
                run_client server_addr
            done
          end;

          (* Stop server *)
          Promise.terminate server
        end
    ]}

    The main program creates a socket for the server and binds it.  The server
    is then started as a new fiber.  Then the clients are started to run
    concurrently.  Finally the server is terminated.

    Finally we run the main program with a scheduler:

    {[
      # Picos_fifos.run main
      Received: Hello!
      Received: Hello!
      Received: Hello!
      Received: Hello!
      Received: Hello!
      - : unit = ()
    ]}

    As an exercise, you might want to refactor the server to avoid
    {{!Finally.move} moving} the file descriptors and use a {{!Finally.let@}
    recursive} accept loop instead.  You could also {{!Bundle.terminate}
    terminate the whole bundle} at the end instead of just terminating the
    server. *)
