(** Syntax for avoiding resource leaks for {!Picos}.

    A resource is something that is {i acquired} and must be {i released} after
    it is no longer needed.

    We open both this library and a few other libraries

    {[
      open Picos_finally
      open Picos_structured
      open Picos_stdio
      open Picos_sync
    ]}

    for the examples. *)

(** {1 API} *)

(** {2 Resource templates} *)

type 'resource template
(** Represents a resource template of release and acquire functions. *)

val finally : ('r -> unit) -> (unit -> 'r) -> 'r template
(** [finally release acquire] returns a resource template with the given
    [release] and [acquire] functions. *)

(** {2 Managed instances}

    A resource instance can be first acquired in one scope and then moved to
    another scope. *)

type 'resource instance
(** Either contains a resource or is empty as the resource has been {{!move}
    moved}, {{!release} released}, or has been {{!let&} borrowed}
    temporarily. *)

val ( let^ ) : 'r template -> ('r instance -> 'a) -> 'a
(** [let^ instance = finally release acquire in scope] calls [acquire ()] to
    obtain a resource and stores it as a resource [instance].  Then, at the end
    of [scope], awaits that the [instance] is {{!move} moved} or {{!release}
    released}, or {{!release} releases} the resource in case an error has been
    raised or the fiber has been canceled.

    💡 In other words, you are expected to either {!move} or {!release} the
    resource.  [let^] only makes sure that the resource is released in case
    something goes wrong. *)

val release : 'r instance -> unit
(** [release instance] releases the resource, if any, contained by the
    [instance].

    @raise Invalid_argument if the resource has been {{!let&} borrowed} and
      hasn't yet been returned. *)

val move : 'r instance -> 'r template
(** [move instance] returns a resource {!template} where the acquire operation
    atomically takes the resource from the [instance] and signals the previous
    owner, and the release operation releases the resource.

    ℹ️ [move] operates lazily such that either party in a transfer may call
    [move].  The transfer takes place atomically at the point the {!template}
    returned by [move] is {{!let@} bound} to a scope.

    @raise Invalid_argument if the resource has been {{!let&} borrowed} and
      hasn't yet been returned, has already been {{!move} moved}, or has been
      {{!release} released} unless the current fiber has been canceled, in which
      case the exception that the fiber was canceled with will be raised. *)

(** {2 Naked resources}

    A naked resource cannot escape the scope in which it was acquired. *)

val ( let@ ) : 'r template -> ('r -> 'a) -> 'a
(** [let@ resource = finally release acquire in scope] calls [acquire ()] to
    obtain a [resource], evaluates [scope], and calls [release resource]
    whether [scope] returns normally or raises an exception. *)

val ( let& ) : 'r instance -> ('r -> 'a) -> 'a
(** [let& resource = instance in scope] borrows the [resource] held by the
    [instance] for the duration of the [scope].

    @raise Invalid_argument if the resource has already been {{!let&} borrowed}
      and hasn't yet been returned, has already been {{!release} released}, or
      has already been {{!move} moved}. *)

(** {1 Examples}

    {2 Recursive server}

    Here is a sketch of a server that recursively forks a fiber to accept and
    handle a client:

    {[
      let recursive_server server_fd =
        Bundle.join_after @@ fun bundle ->

        (* recursive server *)
        let rec accept () =
          let@ client_fd =
            finally Unix.close @@ fun () ->
            Unix.accept ~cloexec:true server_fd
            |> fst
          in

          (* fork to accept other clients *)
          Bundle.fork bundle accept;

          (* handle this client... omitted *)
          ()
        in
        Bundle.fork bundle accept
    ]}

    {2 Looping server}

    There is also a way to {!move} resources to allow forking fibers to handle
    clients without leaks.

    Here is a sketch of a server that accepts in a loop and forks fibers to
    handle clients:

    {[
      let looping_server server_fd =
        Bundle.join_after @@ fun bundle ->

        (* loop to accept clients *)
        while true do
          let^ client_fd =
            finally Unix.close @@ fun () ->
            Unix.accept ~cloexec:true server_fd
            |> fst
          in

          (* fork to handle this client *)
          Bundle.fork bundle @@ fun () ->
            let@ client_fd = move client_fd in

            (* handle client... omitted *)
            ()
        done
    ]}

    {2 Move resource from child to parent}

    You can {!move} a resource between any two fibers and {{!let&} borrow} it
    before moving it.  For example, you can create a resource in a child fiber,
    use it there, and then move it to the parent fiber:

    {[
      let move_from_child_to_parent () =
        Bundle.join_after @@ fun bundle ->

        (* for communicating a resource *)
        let shared_ivar = Ivar.create () in

        (* fork a child that creates a resource *)
        Bundle.fork bundle begin fun () ->
          let pretend_release () = ()
          and pretend_acquire () = () in

          (* allocate a resource *)
          let^ instance =
            finally pretend_release pretend_acquire
          in

          begin
            (* borrow the resource *)
            let& resource = instance in

            (* use the resource... omitted *)
            ()
          end;

          (* send the resource to the parent *)
          Ivar.fill shared_ivar instance
        end;

        (* await for a resource from the child and own it *)
        let@ resource = Ivar.read shared_ivar |> move in

        (* use the resource... omitted *)
        ()
    ]}

    The above uses an {{!Picos_sync.Ivar} [Ivar]} to communicate the movable
    resource from the child fiber to the parent fiber.  Any concurrency safe
    mechanism could be used. *)
