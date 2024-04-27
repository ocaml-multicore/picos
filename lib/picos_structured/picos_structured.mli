(** Basic structured concurrency primitives for {!Picos}.

    This library essentially provides one user level interface for structuring
    fibers with any Picos compatible scheduler.  This library is both meant to
    serve as an example of what can be done and to also provide practical means
    for programming with fibers.  Hopefully there will be many more libraries
    implemented in Picos like this providing different approaches, patterns, and
    idioms for structuring concurrent programs. *)

open Picos

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
          let@ client =
            finally Unix.close @@ fun () ->
            Unix.accept ~cloexec:true socket |> fst
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
          let^ client =
            finally Unix.close @@ fun () ->
            Unix.accept ~closexec:true socket |> fst
          in
          (* fork to handle this client *)
          Bundle.fork bundle @@ fun () ->
            let@ client = move client in
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
      that they should terminate by letting the exception propagate.  This does
      not, by itself, indicate an error. *)

  exception Errors of Exn_bt.t list
  (** An exception that can be used to collect exceptions, typically indicating
      errors, from multiple fibers. *)

  val raise_if_canceled : unit -> unit
  (** [raise_if_canceled ()] checks whether the current fiber has been canceled
      and if so raises the exception that the fiber was canceled with. *)

  val yield : unit -> unit
  (** [yield ()] asks the current fiber to be rescheduled. *)

  val sleep : seconds:float -> unit
  (** [sleep ~seconds] suspends the current fiber for specified number of
      seconds. *)

  val protect : (unit -> 'a) -> 'a
  (** [protect thunk] forbids cancelation for the duration of [thunk ()]. *)

  val block : unit -> 'a
  (** [block ()] suspends the current fiber until it is canceled at which point
      the cancelation exception will be raised.

      ⚠️ Beware that [protect block] never returns and you don't want that. *)
end

module Bundle : sig
  (** A dynamic bundle of fibers guaranteed to be joined at the end. *)

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
      {{!Control.Terminate} [Terminate]} exception.

      The optional [callstack] argument specifies the number of callstack
      entries to capture with the {{!Control.Terminate} [Terminate]} exception.
      The default is [0].  *)

  val error : t -> Exn_bt.t -> unit
  (** [error bundle exn_bt] first calls {!terminate} and then adds the exception
      with backtrace to the list of exceptions to be raised, unless the
      exception is the {{!Control.Terminate} [Terminate]} exception, which is
      not considered to signal an error by itself. *)

  val fork : t -> (unit -> unit) -> unit
  (** [fork bundle action] spawns a new fiber to the [bundle] that will run the
      given [action].  If the action raises an exception, {!error} will be
      called with that exception. *)
end

module Run : sig
  (** Operations for running fibers in specific patterns. *)

  val all : (unit -> unit) list -> unit
  (** [all actions] starts all the actions as separate fibers and waits until
      they all complete. *)

  val any : (unit -> unit) list -> unit
  (** [any actions] starts all the actions as separate fibers and waits until at
      least one of them completes.  The rest of the started fibers will then be
      canceled.

      ⚠️ Calling [any []] is equivalent to calling
      {{!Control.block} [block ()]}. *)
end

(** {1 Examples}

    First we open some modules for convenience:

    {[
      open Picos_structured.Finally
      open Picos_structured
      open Picos_stdio
      open Picos_sync
    ]}

    {2 A simple echo server and clients}

    Let's build a simple TCP echo server and run it with some clients.

    We first define a function for the server:

    {[
      let server socket =
        Unix.listen socket 8;

        Bundle.join_after begin fun bundle ->
          while true do
            let^ client =
              finally Unix.close @@ fun () ->
              Unix.accept
                ~cloexec:true socket |> fst
            in

            (* Fork a fiber for client *)
            Bundle.fork bundle begin fun () ->
              let@ client = move client in
              Unix.set_nonblock client;

              let bytes =
                Bytes.create 100
              in
              let n =
                Unix.read client bytes 0
                  (Bytes.length bytes)
              in
              Unix.write client bytes 0 n
              |> ignore
            end
          done
        end
    ]}

    The server function expects a bound socket and starts listening.  For each
    accepted client the server forks a new fiber to handle it.

    Let's then define a function for the clients:

    {[
      let client addr =
        let@ socket =
          finally Unix.close @@ fun () ->
          Unix.socket ~cloexec:true
            PF_INET SOCK_STREAM 0
        in
        Unix.set_nonblock socket;
        Unix.connect socket addr;

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
    the server address.  It then writes a message to server and reads a reply
    from the server and prints it.

    Here is the main program:

    {[
      let main () =
        let@ socket =
          finally Unix.close @@ fun () ->
          Unix.socket ~cloexec:true
            PF_INET SOCK_STREAM 0
        in
        Unix.set_nonblock socket;

        (* Let system determine the port *)
        Unix.bind socket Unix.(
          ADDR_INET(inet_addr_loopback, 0));

        let addr =
          Unix.getsockname socket
        in

        Bundle.join_after begin fun bundle ->
          (* Start server *)
          Bundle.fork bundle (fun () ->
            server socket);

          (* Run clients concurrently *)
          Bundle.join_after begin fun bundle ->
            for _=1 to 5 do
              Bundle.fork bundle (fun () ->
                client addr)
            done
          end;

          (* Stop server *)
          Bundle.terminate bundle
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
    recursive} accept loop instead. *)
