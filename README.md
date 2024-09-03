[API reference](https://ocaml-multicore.github.io/picos/doc/index.html) &middot;
[Benchmarks](https://bench.ci.dev/ocaml-multicore/picos/branch/main?worker=pascal&image=bench.Dockerfile)
&middot;
[Stdlib Benchmarks](https://bench.ci.dev/ocaml-multicore/multicore-bench/branch/main?worker=pascal&image=bench.Dockerfile)

# **Picos** &mdash; Interoperable effects based concurrency

Picos is a
[systems programming](https://en.wikipedia.org/wiki/Systems_programming)
interface between effects based schedulers and concurrent abstractions.

<p align="center"><a href="https://ocaml-multicore.github.io/picos/doc/picos/index.html"><img width="65%" src="https://raw.githubusercontent.com/ocaml-multicore/picos/main/doc/picos-ecosystem.svg"></a></p>

Picos is designed to enable an _open ecosystem_ of
[interoperable](https://en.wikipedia.org/wiki/Interoperability) and
interchangeable elements of effects based cooperative concurrent programming
models such as

- [schedulers](<https://en.wikipedia.org/wiki/Scheduling_(computing)>) that
  multiplex large numbers of
  [user level fibers](https://en.wikipedia.org/wiki/Green_thread) to run on a
  small number of system level threads,
- mechanisms for managing fibers and for
  [structuring concurrency](https://en.wikipedia.org/wiki/Structured_concurrency),
- communication and synchronization primitives, such as
  [mutexes and condition variables](<https://en.wikipedia.org/wiki/Monitor_(synchronization)>),
  message queues,
  [STM](https://en.wikipedia.org/wiki/Software_transactional_memory)s, and more,
  and
- integrations with low level
  [asynchronous IO](https://en.wikipedia.org/wiki/Asynchronous_I/O) systems

by decoupling such elements from each other.

Picos comes with a
[reference manual](https://ocaml-multicore.github.io/picos/doc/index.html) and
many sample libraries.

⚠️ Please note that Picos is still considered experimental and unstable.

## Introduction

Picos addresses the incompatibility of effects based schedulers at a fundamental
level by introducing
[an _interface_ to decouple schedulers and other concurrent abstractions](https://ocaml-multicore.github.io/picos/doc/picos/Picos/index.html)
that need services from a scheduler.

The
[core abstractions of Picos](https://ocaml-multicore.github.io/picos/doc/picos/Picos/index.html#the-architecture-of-picos)
are

- [`Trigger`](https://ocaml-multicore.github.io/picos/doc/picos/Picos/Trigger/index.html)
  — the ability to await for a signal,
- [`Computation`](https://ocaml-multicore.github.io/picos/doc/picos/Picos/Computation/index.html)
  — a cancelable computation, and
- [`Fiber`](https://ocaml-multicore.github.io/picos/doc/picos/Picos/Fiber/index.html)
  — an independent thread of execution,

that are implemented partially by the Picos interface in terms of the effects

- [`Trigger.Await`](https://ocaml-multicore.github.io/picos/doc/picos/Picos/Trigger/index.html#extension-Await)
  — to suspend and resume a fiber,
- [`Computation.Cancel_after`](https://ocaml-multicore.github.io/picos/doc/picos/Picos/Computation/index.html#extension-Cancel_after)
  — to cancel a computation after given period of time,
- [`Fiber.Current`](https://ocaml-multicore.github.io/picos/doc/picos/Picos/Fiber/index.html#extension-Current)
  — to obtain the current fiber,
- [`Fiber.Yield`](https://ocaml-multicore.github.io/picos/doc/picos/Picos/Fiber/index.html#extension-Yield)
  — to request rescheduling, and
- [`Fiber.Spawn`](https://ocaml-multicore.github.io/picos/doc/picos/Picos/Fiber/index.html#extension-Spawn)
  — to start a new fiber.

The partial implementation of the abstractions and the effects define a contract
between schedulers and other concurrent abstractions. By handling the Picos
effects according to the contract a scheduler becomes _Picos compatible_, which
allows any abstractions written against the Picos interface, i.e. _Implemented
in Picos_, to be used with the scheduler.

### Understanding cancelation

A central idea or goal of Picos is to provide a collection of building blocks
for parallelism safe cancelation that allows the implementation of both blocking
abstractions as well as the implementation of abstractions for structuring
fibers for cancelation or managing the propagation and scope of cancelation.

While cancelation, which is essentially a kind of asynchronous exception or
signal, is not necessarily recommended as a general control mechanism, the
ability to cancel fibers in case of errors is crucial for the implementation of
practical concurrent programming models.

Consider the following characteristic
[example](https://ocaml-multicore.github.io/picos/doc/picos_std/Picos_std_structured/index.html#understanding-cancelation):

```ocaml skip
Mutex.protect mutex begin fun () ->
  while true do
    Condition.wait condition mutex
  done
end
```

Assume that a fiber executing the above code might be canceled, at any point, by
another fiber running in parallel. This could be necessary, for example, due to
an error that requires the application to be shut down. How could that be done
while ensuring both
[safety and liveness](https://en.wikipedia.org/wiki/Safety_and_liveness_properties)?

- For safety, cancelation should not leave the program in an invalid state or
  cause the program to leak memory. In this case, `Condition.wait` must exit
  with the mutex locked, even in case of cancelation, and, as `Mutex.protect`
  exits, the ownership of the mutex must be transferred to the next fiber, if
  any, waiting in queue for the mutex. No references to unused objects may be
  left in the mutex or the condition variable.

- For liveness, cancelation should ensure that the fiber will eventually
  continue after cancelation. In this case, cancelation could be triggered
  during the `Mutex.lock` operation inside `Mutex.protect` or the
  `Condition.wait` operation, when the fiber might be in a suspended state, and
  cancelation should then allow the fiber to continue.

The set of abstractions, `Trigger`, `Computation`, and `Fiber`, work together
[to support cancelation](https://ocaml-multicore.github.io/picos/doc/picos/Picos/index.html#cancelation-in-picos).
Briefly, a fiber corresponds to an independent thread of execution and every
fiber is associated with a computation at all times. When a fiber creates a
trigger in order to await for a signal, it ask the scheduler to suspend the
fiber on the trigger. Assuming the fiber has not forbidden the propagation of
cancelation, which is required, for example, in the implementation of
`Condition.wait` to lock the mutex upon exit, the scheduler must also attach the
trigger to the computation associated with the fiber. If the computation is then
canceled before the trigger is otherwise signaled, the trigger will be signaled
by the cancelation of the computation, and the fiber will be resumed by the
scheduler as canceled.

This cancelable suspension protocol and its partial implementation designed
around the first-order
[`Trigger.Await`](https://ocaml-multicore.github.io/picos/doc/picos/Picos/Trigger/index.html#extension-Await)
effect creates a clear separation between schedulers and user code running in
fibers and is designed to handle the possibility of a trigger being signaled or
a computation being canceled at any point during the suspension of a fiber.
Schedulers are given maximal freedom to decide which fiber to resume next. As an
example, a scheduler could give priority to canceled fibers &mdash; going as far
as moving a fiber already in the ready queue of the scheduler to the front of
the queue at the point of cancelation &mdash; based on the assumption that user
code promptly cancels external requests and frees critical resources.

### `Trigger`

A trigger provides the ability to await for a signal and is perhaps the best
established and least controversial element of the Picos interface.

Here is an extract from the signature of the
[`Trigger` module](https://ocaml-multicore.github.io/picos/doc/picos/Picos/Trigger/index.html):

<!--
```ocaml
# open Picos
# open Picos_std_finally
# open Picos_std_structured
# open Picos_std_sync
```
-->

```ocaml skip
type t
val create : unit -> t
val await : t -> (exn * Printexc.raw_backtrace) option
val signal : t -> unit
val on_signal : (* for schedulers *)
```

The idea is that a fiber may create a trigger, insert it into some shared data
structure, and then call `await` to ask the scheduler to suspend the fiber until
something signals the trigger. When `await` returns an exception with a
backtrace it means that the fiber has been canceled.

As an example, let's consider the implementation of an `Ivar` or incremental or
single-assignment variable:

```ocaml skip
type 'a t
val create : unit -> 'a t
val try_fill : 'a t -> 'a -> bool
val read : 'a t -> 'a
```

An `Ivar` is created as empty and can be filled with a value once. An attempt to
read an `Ivar` blocks until the `Ivar` is filled.

Using `Trigger` and `Atomic`, we can represent an `Ivar` as follows:

```ocaml
type 'a state =
  | Filled of 'a
  | Empty of Trigger.t list

type 'a t = 'a state Atomic.t
```

The `try_fill` operation is then fairly straightforward to implement:

```ocaml
let rec try_fill t value =
  match Atomic.get t with
  | Filled _ -> false
  | Empty triggers as before ->
    let after = Filled value in
    if Atomic.compare_and_set t before after then
      begin
        List.iter Trigger.signal triggers; (* ! *)
        true
      end
    else
      try_fill t value
```

The interesting detail above is that after successfully filling an `Ivar`, the
triggers are signaled. This allows the `await` inside the `read` operation to
return:

<!--
```ocaml
let cleanup _t _trigger = ()
```
--->

```ocaml
let rec read t =
  match Atomic.get t with
  | Filled value -> value
  | Empty triggers as before ->
    let trigger = Trigger.create () in
    let after = Empty (trigger :: triggers) in
    if Atomic.compare_and_set t before after then
      match Trigger.await trigger with
      | None -> read t
      | Some (exn, bt) ->
        cleanup t trigger; (* ! *)
        Printexc.raise_with_backtrace exn bt
    else
      read t
```

An important detail above is that when `await` returns an exception with a
backtrace, meaning that the fiber has been canceled, the `cleanup` operation
(which is omitted) is called to remove the `trigger` from the `Ivar` to avoid
potentially accumulating unbounded numbers of triggers in an empty `Ivar`.

As simple as it is, the design of `Trigger` is far from arbitrary:

- First of all, `Trigger` has single-assignment semantics. After being signaled,
  a trigger takes a constant amount of space and does not point to any other
  heap object. This makes it easier to reason about the behavior and can also
  help to avoid leaks or optimize data structures containing triggers, because
  it is safe to hold bounded amounts of signaled triggers.

- The `Trigger` abstraction is essentially first-order, which provides a clear
  separation between a scheduler and programs, or fibers, running on a
  scheduler. The `await` operation performs the `Await` effect, which passes the
  trigger to the scheduler. The scheduler then attaches its own callback to the
  trigger using `on_signal`. This way a scheduler does not call arbitrary user
  specified code in the `Await` effect handler.

- Separating the creation of a trigger from the `await` operation allows one to
  easily insert a trigger into any number of places and allows the trigger to be
  potentially concurrently signaled before the `Await` effect is performed in
  which case the effect can be skipped entirely.

- No value is propagated with a trigger. This makes triggers simpler and makes
  it less likely for one to e.g. accidentally drop such a value. In many cases,
  like with the `Ivar`, there is already a data structure through which values
  can be propagated.

- The `signal` operation gives no indication of whether a fiber will then be
  resumed as canceled or not. This gives maximal flexibility for the scheduler
  and also makes it clear that cancelation must be handled based on the return
  value of `await`.

### `Computation`

A `Computation` basically holds the status, i.e. _running_, _returned_, or
_canceled_, of some sort of computation and allows anyone with access to the
computation to attach triggers to it to be signaled in case the computation
stops running.

Here is an extract from the signature of the
[`Computation` module](https://ocaml-multicore.github.io/picos/doc/picos/Picos/Computation/index.html):

```ocaml skip
type 'a t

val create : unit -> 'a t

val try_attach : 'a t -> Trigger.t -> bool
val detach : 'a t -> Trigger.t -> unit

val try_return : 'a t -> 'a -> bool
val try_cancel : 'a t -> exn -> Printexc.raw_backtrace -> bool

val check : 'a t -> unit
val await : 'a t -> 'a
```

A `Computation` directly provides a superset of the functionality of the `Ivar`
we sketched in the previous section:

```ocaml
type 'a t = 'a Computation.t
let create : unit -> 'a t = Computation.create
let try_fill : 'a t -> 'a -> bool =
  Computation.try_return
let read : 'a t -> 'a = Computation.await
```

However, what really makes the `Computation` useful is the ability to
momentarily attach triggers to it. A `Computation` essentially implements a
specialized lock-free bag of triggers, which allows one to implement dynamic
completion propagation networks.

The `Computation` abstraction is also designed with both simplicity and
flexibility in mind:

- Similarly to `Trigger`, `Computation` has single-assignment semantics, which
  makes it easier to reason about.

- Unlike a typical cancelation context of a structured concurrency model,
  `Computation` is unopinionated in that it does not impose a specific
  hierarchical structure.

- Anyone may ask to be notified when a `Computation` is completed by attaching
  triggers to it and anyone may complete a `Computation`. This makes
  `Computation` an omnidirectional communication primitive.

Interestingly, and unintentionally, it turns out that, given
[the ability to complete two (or more) computations atomically](https://ocaml-multicore.github.io/picos/doc/picos/Picos/Computation/Tx/index.html),
`Computation` is essentially expressive enough to implement the
[event](https://ocaml.org/manual/latest/api/Event.html) abstraction of
[Concurrent ML](https://en.wikipedia.org/wiki/Concurrent_ML). The same features
that make `Computation` suitable for implementing more or less arbitrary dynamic
completion propagation networks make it suitable for implementing Concurrent ML
style abstractions.

### `Fiber`

A fiber corresponds to an independent thread of execution. Technically an
effects based scheduler creates a fiber, effectively giving it an identity, as
it runs some function under its handler. The `Fiber` abstraction provides a way
to share a proxy identity, and a bit of state, between a scheduler and other
concurrent abstractions.

Here is an extract from the signature of the
[`Fiber` module](https://ocaml-multicore.github.io/picos/doc/picos/Picos/Fiber/index.html):

```ocaml skip
type t

val current : unit -> t

val create : forbid:bool -> 'a Computation.t -> t
val spawn : t -> (t -> unit) -> unit

val get_computation : t -> Computation.packed
val set_computation : t -> Computation.packed -> unit

val has_forbidden : t -> bool
val exchange : t -> forbid:bool -> bool

module FLS : sig (* ... *) end
```

Fibers are where all of the low level bits and pieces of Picos come together,
which makes it difficult to give both meaningful and concise examples, but let's
implement a slightly simplistic structured concurrency mechanism:

```ocaml skip
type t (* represents a scope *)
val run : (t -> unit) -> unit
val fork : t -> (unit -> unit) -> unit
```

The idea here is that `run` creates a "scope" and waits until all of the fibers
forked into the scope have finished. In case any fiber raises an unhandled
exception, or the main fiber that created the scope is canceled, all of the
fibers are canceled and an exception is raised. To keep things slightly simpler,
only the first exception is kept.

A scope can be represented by a simple record type:

```ocaml
type t = {
  count : int Atomic.t;
  inner : unit Computation.t;
  ended : Trigger.t;
}
```

The idea is that after a fiber is finished, we decrement the count and if it
becomes zero, we finish the computation and signal the main fiber that the scope
has ended:

```ocaml
let decr t =
  let n = Atomic.fetch_and_add t.count (-1) in
  if n = 1 then begin
    Computation.finish t.inner;
    Trigger.signal t.ended
  end
```

When forking a fiber, we increment the count unless it already was zero, in
which case we raise an error:

```ocaml
let rec incr t =
  let n = Atomic.get t.count in
  if n = 0 then invalid_arg "ended";
  if not (Atomic.compare_and_set t.count n (n + 1))
  then incr t
```

The fork operation is now relatively straightforward to implement:

```ocaml
let fork t action =
  incr t;
  try
    let main _ =
      match action () with
      | () -> decr t
      | exception exn ->
          let bt = Printexc.get_raw_backtrace () in
          Computation.cancel t.inner exn bt;
          decr t
    in
    let fiber =
      Fiber.create ~forbid:false t.inner
    in
    Fiber.spawn fiber main
  with canceled_exn ->
    decr t;
    raise canceled_exn
```

The above `fork` first increments the count and then tries to spawn a fiber. The
Picos interface specifies that when `Fiber.spawn` returns normally, the action,
`main`, must be called by the scheduler. This allows us to ensure that the
increment is always matched with a decrement.

Setting up a scope is the most complex operation:

<!--
```ocaml
let join _ _ _ _ = ()
```
-->

```ocaml
let run body =
  let count = Atomic.make 1 in
  let inner = Computation.create () in
  let ended = Trigger.create () in
  let t = { count; inner; ended } in
  let fiber = Fiber.current () in
  let (Packed outer) =
    Fiber.get_computation fiber
  in
  let canceler =
    Computation.attach_canceler
      ~from:outer
      ~into:t.inner
  in
  match
    Fiber.set_computation fiber (Packed t.inner);
    body t
  with
  | () -> join t outer canceler fiber
  | exception exn ->
      let bt = Printexc.get_raw_backtrace () in
      Computation.cancel t.inner exn bt;
      join t outer canceler fiber;
      Printexc.raise_with_backtrace exn bt
```

The `Computation.attach_canceler` operation attaches a special trigger to
propagate cancelation from one computation into another. After the body exits,
`join`

```ocaml
let join t outer canceler fiber =
  decr t;
  Fiber.set_computation fiber (Packed outer);
  let forbid = Fiber.exchange fiber ~forbid:true in
  Trigger.await t.ended |> ignore;
  Fiber.set fiber ~forbid;
  Computation.detach outer canceler;
  Computation.check t.inner;
  Fiber.check fiber
```

is called to wait for the scoped fibers and restore the state of the main fiber.
An important detail is that propagation of cancelation is forbidden by setting
the `forbid` flag to `true` before the call of `Trigger.await`. This is
necessary to ensure that `join` does not exit, due to the fiber being canceled,
before all of the child fibers have actually finished. Finally, `join` checks
the inner computation and the fiber, which means that an exception will be
raised in case either was canceled.

The design of `Fiber` includes several key features:

- The low level design allows one to both avoid unnecessary overheads, such as
  allocating a `Computation.t` for every fiber, when implementing simple
  abstractions and also to implement more complex behaviors that might prove
  difficult given e.g. a higher level design with a built-in notion of
  hierarchy.

- As `Fiber.t` stores the `forbid` flag and the `Computation.t` associated with
  the fiber one need not pass those as arguments through the program. This
  allows various concurrent abstractions to be given traditional interfaces,
  which would otherwise need to be complicated.

- Effects are relatively expensive. The cost of performing effects can be
  amortized by obtaining the `Fiber.t` once and then manipulating it multiple
  times.

- A `Fiber.t` also provides an identity for the fiber. It has so far proven to
  be sufficient for most purposes. Fiber local storage, which we do not cover
  here, can be used to implement, for example, a unique integer id for fibers.

### Assumptions

Now, consider the `Ivar` abstraction presented earlier as an example of the use
of the `Trigger` abstraction. That `Ivar` implementation, as well as the
`Computation` based implementation, works exactly as desired inside the scope
abstraction presented in the previous section. In particular, a blocked
`Ivar.read` can be canceled, either when another fiber in a scope raises an
unhandled exception or when the main fiber of the scope is canceled, which
allows the fiber to continue by raising an exception after cleaning up. In fact,
Picos comes with a number of libraries that all would work quite nicely with the
examples presented here.

For example, a library provides an operation to run a block with a timeout on
the current fiber. One could use it with `Ivar.read` to implement a read
operation
[with a timeout](https://ocaml-multicore.github.io/picos/doc/picos_std/Picos_std_structured/Control/index.html#val-terminate_after):

```ocaml
let read_in ~seconds ivar =
  Control.terminate_after ~seconds @@ fun () ->
  Ivar.read ivar
```

This interoperability is not accidental. For example, the scope abstraction
basically assumes that one does not use `Fiber.set_computation`, in an arbitrary
unscoped manner inside the scoped fibers. An idea with the Picos interface
actually is that it is not supposed to be used by applications at all and most
higher level libraries should be built on top of libraries that do not directly
expose elements of the Picos interface.

Perhaps more interestingly, there are obviously limits to what can be achieved
in an "interoperable" manner. Imagine an operation like

```ocaml skip
val at_exit : (unit -> unit) -> unit
```

that would allow one to run an action just before a fiber exits. One could, of
course, use a custom spawn function that would support such cleanup, but then
`at_exit` could only be used on fibers spawned through that particular spawn
function.

### The effects

As mentioned previously, the Picos interface is implemented partially in terms
of five effects:

```ocaml version>=5.0.0
type _ Effect.t +=
  | Await : Trigger.t -> (exn * Printexc.raw_backtrace) option Effect.t
  | Cancel_after : {
      seconds : float;
      exn: exn;
      bt : Printexc.raw_backtrace;
      computation : 'a Computation.t;
    }
      -> unit Effect.t
  | Current : t Effect.t
  | Yield : unit Effect.t
  | Spawn : {
      fiber : Fiber.t;
      main : (Fiber.t -> unit);
    }
      -> unit Effect.t
```

A scheduler must handle those effects as specified in the Picos documentation.

The Picos interface does not, in particular, dictate which ready fibers a
scheduler must run next and on which domains. Picos also does not require that a
fiber should stay on the domain on which it was spawned. Abstractions
implemented against the Picos interface should not assume any particular
scheduling.

Picos actually comes with
[a randomized multithreaded scheduler](https://ocaml-multicore.github.io/picos/doc/picos_std/Picos_std_randos/index.html),
that, after handling any of the effects, picks the next ready fiber randomly. It
has proven to be useful for testing that abstractions implemented in Picos do
not make invalid scheduling assumptions.

When a concurrent abstraction requires a particular scheduling, it should
primarily be achieved through the use of synchronization abstractions like when
programming with traditional threads. Application programs may, of course, pick
specific schedulers.

## Status and results

We have an experimental design and implementation of the core Picos interface as
illustrated in the previous section. We have also created several _Picos
compatible_
[sample schedulers](https://ocaml-multicore.github.io/picos/doc/picos_mux/index.html).
A scheduler, in this context, just multiplexes fibers to run on one or more
system level threads. We have also created some sample higher-level
[scheduler agnostic libraries](https://ocaml-multicore.github.io/picos/doc/picos_std/index.html)
_Implemented in Picos_. These libraries include
[a library for resource management](https://ocaml-multicore.github.io/picos/doc/picos_std/Picos_std_finally/index.html),
[a library for structured concurrency](https://ocaml-multicore.github.io/picos/doc/picos_std/Picos_std_structured/index.html),
[a library of synchronization primitives](https://ocaml-multicore.github.io/picos/doc/picos_std/Picos_std_sync/index.html),
and
[an asynchronous I/O library](https://ocaml-multicore.github.io/picos/doc/picos_io/Picos_io/index.html).
The synchronization library and the I/O library intentionally mimic libraries
that come with the OCaml distribution. All of the libraries work with all of the
schedulers and all of these _elements_ are interoperable and entirely opt-in.

What is worth explicitly noting is that all of these schedulers and libraries
are small, independent, and highly modular pieces of code. They all crucially
depend on and are decoupled from each other via the core Picos interface
library. A basic single threaded scheduler implementation requires only about
100 lines of code (LOC). A more complex parallel scheduler might require a
couple of hundred LOC. The scheduler agnostic libraries are similarly small.

Here is an
[example](https://ocaml-multicore.github.io/picos/doc/picos_std/Picos_std_structured/index.html#a-simple-echo-server-and-clients)
of a concurrent echo server using the scheduler agnostic libraries provided as
samples:

```ocaml
let run_server server_fd =
  Unix.listen server_fd 8;
  Flock.join_after begin fun () ->
    while true do
      let@ client_fd = instantiate Unix.close @@ fun () ->
        Unix.accept ~cloexec:true server_fd |> fst
      in
      Flock.fork begin fun () ->
        let@ client_fd = move client_fd in
        Unix.set_nonblock client_fd;
        let bs = Bytes.create 100 in
        let n =
          Unix.read client_fd bs 0 (Bytes.length bs)
        in
        Unix.write client_fd bs 0 n |> ignore
      end
    done
  end
```

The
[`Unix`](https://ocaml-multicore.github.io/picos/doc/picos_io/Picos_io/Unix/index.html)
module is provided by the I/O library. The operations on file descriptors on
that module, such as `accept`, `read`, and `write`, use the Picos interface to
suspend fibers allowing other fibers to run while waiting for I/O. The
[`Flock`](https://ocaml-multicore.github.io/picos/doc/picos_std/Picos_std_structured/Flock/index.html)
module comes from the structured concurrency library. A call of
[`join_after`](https://ocaml-multicore.github.io/picos/doc/picos_std/Picos_std_structured/Flock/index.html#val-join_after)
returns only after all the fibers
[`fork`](https://ocaml-multicore.github.io/picos/doc/picos_std/Picos_std_structured/Flock/index.html#val-fork)ed
into the flock have terminated. If the main fiber of the flock is canceled, or
any fiber within the flock raises an unhandled exception, all the fibers within
the flock will be canceled and an exception will be raised on the main fiber of
the flock. The
[`let@`](https://ocaml-multicore.github.io/picos/doc/picos_std/Picos_std_finally/index.html#val-let@),
[`finally`](https://ocaml-multicore.github.io/picos/doc/picos_std/Picos_std_finally/index.html#val-instantiate),
and
[`move`](https://ocaml-multicore.github.io/picos/doc/picos_std/Picos_std_finally/index.html#val-move)
operations come from the resource management library and allow dealing with
resources in a leak-free manner. The responsibility to close the `client_fd`
socket is
[`move`](https://ocaml-multicore.github.io/picos/doc/picos_std/Picos_std_finally/index.html#val-move)d
from the main server fiber to a fiber forked to handle that client.

We should emphasize that the above is just an example. The Picos interface
should be both expressive and efficient enough to support practical
implementations of many different kinds of concurrent programming models. Also,
as described previously, the Picos interface does not, for example, internally
implement structured concurrency. However, the abstractions provided by Picos
are designed to allow structured and unstructured concurrency to be _Implemented
in Picos_ as libraries that will then work with any _Picos compatible_ scheduler
and with other concurrent abstractions.

Finally, an interesting demonstration that Picos really fundamentally is an
interface is
[a prototype _Picos compatible_ direct style interface to Lwt](https://ocaml-multicore.github.io/picos/doc/picos_lwt/Picos_lwt/index.html).
The implementation uses shallow effect handlers and defers all scheduling
decisions to Lwt. Running a program with the scheduler returns a Lwt promise.

## Future work

As mentioned previously, Picos is still an ongoing project and the design is
considered experimental. We hope that Picos soon matures to serve the needs of
both the commercial users of OCaml and the community at large.

Previous sections already touched a couple of updates currently in development,
such as the support for finalizing resources stored in
[`FLS`](https://ocaml-multicore.github.io/picos/doc/picos/Picos/Fiber/FLS/index.html)
and the development of Concurrent ML style abstractions. We also have ongoing
work to formalize aspects of the Picos interface.

One potential change we will be investigating is whether the
[`Computation`](https://ocaml-multicore.github.io/picos/doc/picos/Picos/Computation/index.html)
abstraction should be simplified to only support cancelation.

The implementation of some operations, such as
[`Fiber.current`](https://ocaml-multicore.github.io/picos/doc/picos/Picos/Fiber/index.html#val-current)
to retrieve the current fiber proxy identity, do not strictly need to be
effects. Performing an effect is relatively expensive and we will likely design
a mechanism to store a reference to the current fiber in some sort of local
storage, which could significantly improve the performance of certain
abstractions, such as checked mutexes, that need to access the current fiber.

We also plan to develop a minimalist library for spawning threads over domains,
much like Moonpool, in a cooperative manner for schedulers and other libraries.

We also plan to make Domainslib Picos compatible, which will require developing
a more efficient non-effects based interface for spawning fibers, and
investigate making Eio Picos compatible.

We also plan to design and implement asynchronous IO libraries for Picos using
various system call interface for asynchronous IO such as io_uring.

Finally, Picos is supposed to be an _open ecosystem_. If you have feedback or
would like to work on something mentioned above, let us know.

## Motivation

There are already several concrete effects-based concurrent programming
libraries and models being developed. Here is a list of some such publicly
available projects:<sup>[\*](https://xkcd.com/927/)</sup>

1. [Affect](https://github.com/dbuenzli/affect) — "Composable concurrency
   primitives with OCaml effects handlers (unreleased)",
2. [Domainslib](https://github.com/ocaml-multicore/domainslib) —
   "Nested-parallel programming",
3. [Eio](https://github.com/ocaml-multicore/eio) — "Effects-Based Parallel IO
   for OCaml",
4. [Fuseau](https://github.com/c-cube/fuseau) — "Lightweight fiber library for
   OCaml 5",
5. [Miou](https://github.com/robur-coop/miou) — "A simple scheduler for OCaml
   5",
6. [Moonpool](https://github.com/c-cube/moonpool) — "Commodity thread pools for
   OCaml 5", and
7. [Riot](https://github.com/leostera/riot) — "An actor-model multi-core
   scheduler for OCaml 5".

All of the above libraries are mutually incompatible with each other with the
exception that Domainslib, Eio, and Moonpool implement an earlier
interoperability proposal called
[domain-local-await](https://github.com/ocaml-multicore/domain-local-await/) or
DLA, which allows a concurrent programming library like
[Kcas](https://github.com/ocaml-multicore/kcas/)[\*](https://github.com/ocaml-multicore/kcas/pull/136)
to work on all of those. Unfortunately, DLA, by itself, is known to be
insufficient and the design has not been universally accepted.

By introducing a scheduler interface and key libraries, such as an IO library,
implemented on top of the interface, we hope that the scarce resources of the
OCaml community are not further divided into mutually incompatible ecosystems
built on top of such mutually incompatible concurrent programming libraries,
while, simultaneously, making it possible to experiment with many kinds of
concurrent programming models.

It should be
technically<sup>[\*](https://www.youtube.com/watch?v=hou0lU8WMgo)</sup> possible
for all the previously mentioned libraries, except
[Miou](https://github.com/robur-coop/miou), to

1. be made
   [Picos compatible](https://ocaml-multicore.github.io/picos/doc/picos/index.html#picos-compatible),
   i.e. to handle the Picos effects, and
2. have their elements
   [implemented in Picos](https://ocaml-multicore.github.io/picos/doc/picos/index.html#implemented-in-picos),
   i.e. to make them usable on other Picos-compatible schedulers.

Please read
[the reference manual](https://ocaml-multicore.github.io/picos/doc/index.html)
for further information.
