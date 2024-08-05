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

(** {2 Basics} *)

val ( let@ ) : ('a -> 'b) -> 'a -> 'b
(** [let@ resource = template in scope] is equivalent to
    [template (fun resource -> scope)].

    ℹ️ You can use this binding operator with any [template] function that has a
    type of the form [('r -> 'a) -> 'a]. *)

val finally : ('r -> unit) -> (unit -> 'r) -> ('r -> 'a) -> 'a
(** [finally release acquire scope] calls [acquire ()] to obtain a [resource],
    calls [scope resource], and then calls [release resource] after the scope
    exits. *)

(** {2 Instances} *)

type 'r instance
(** Either contains a resource or is empty as the resource has been {{!transfer}
    transferred}, {{!drop} dropped}, or has been {{!borrow} borrowed}
    temporarily. *)

val instantiate : ('r -> unit) -> (unit -> 'r) -> ('r instance -> 'a) -> 'a
(** [instantiate release acquire scope] calls [acquire ()] to obtain a resource
    and stores it as an {!instance}, calls [scope instance].  Then, if [scope]
    returns normally, awaits until the {!instance} becomes empty.  In case
    [scope] raises an exception or the fiber is canceled, the instance will be
    {{!drop} dropped}. *)

val drop : 'r instance -> unit
(** [drop instance] releases the resource, if any, contained by the {!instance}.

    @raise Invalid_argument if the resource has been {{!let&} borrowed} and
      hasn't yet been returned. *)

val borrow : 'r instance -> ('r -> 'a) -> 'a
(** [borrow instance scope] borrows the [resource] stored in the [instance],
    calls [scope resource], and then returns the [resource] to the [instance]
    after scope exits.

    @raise Invalid_argument if the resource has already been {{!borrow}
      borrowed} and hasn't yet been returned, has already been {{!drop}
      dropped}, or has already been {{!transfer} transferred}. *)

val transfer : 'r instance -> ('r instance -> 'a) -> 'a
(** [transfer source] transfers the [resource] stored in the [source] instance
    into a new [target] instance, calls [scope target].  Then, if [scope]
    returns normally, awaits until the [target] instance becomes empty.  In case
    [scope] raises an exception or the fiber is canceled, the [target] instance
    will be {{!drop} dropped}.

    @raise Invalid_argument if the resource has been {{!borrow} borrowed} and
      hasn't yet been returned, has already been {{!transfer} transferred}, or
      has been {{!drop} dropped} unless the current fiber has been canceled, in
      which case the exception that the fiber was canceled with will be
      raised. *)

val move : 'r instance -> ('r -> 'a) -> 'a
(** [move instance scope] is equivalent to
    {{!transfer} [transfer instance (fun instance -> borrow instance scope)]}. *)

(** {1 Examples}

    {2 Recursive server}

    Here is a sketch of a server that recursively forks a fiber to accept and
    handle a client:

    {[
      let recursive_server server_fd =
        Flock.join_after @@ fun () ->

        (* recursive server *)
        let rec accept () =
          let@ client_fd =
            finally Unix.close @@ fun () ->
            Unix.accept ~cloexec:true server_fd
            |> fst
          in

          (* fork to accept other clients *)
          Flock.fork accept;

          (* handle this client... omitted *)
          ()
        in
        Flock.fork accept
    ]}

    {2 Looping server}

    There is also a way to {{!move} move} {{!instantiate} instantiated}
    resources to allow forking fibers to handle clients without leaks.

    Here is a sketch of a server that accepts in a loop and forks fibers to
    handle clients:

    {[
      let looping_server server_fd =
        Flock.join_after @@ fun () ->

        (* loop to accept clients *)
        while true do
          let@ client_fd =
            instantiate Unix.close @@ fun () ->
            Unix.accept ~cloexec:true server_fd
            |> fst
          in

          (* fork to handle this client *)
          Flock.fork @@ fun () ->
            let@ client_fd = move client_fd in

            (* handle client... omitted *)
            ()
        done
    ]}

    {2 Move resource from child to parent}

    You can {{!move} move} an {{!instantiate} instantiated} resource between any
    two fibers and {{!borrow} borrow} it before moving it.  For example, you can
    create a resource in a child fiber, use it there, and then move it to the
    parent fiber:

    {[
      let move_from_child_to_parent () =
        Flock.join_after @@ fun () ->

        (* for communicating a resource *)
        let shared_ivar = Ivar.create () in

        (* fork a child that creates a resource *)
        Flock.fork begin fun () ->
          let pretend_release () = ()
          and pretend_acquire () = () in

          (* allocate a resource *)
          let@ instance =
            instantiate pretend_release pretend_acquire
          in

          begin
            (* borrow the resource *)
            let@ resource = borrow instance in

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
