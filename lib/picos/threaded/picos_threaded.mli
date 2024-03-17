(** A basic [Thread] based implementation of the effects based operations of
    {!Picos}.

    Briefly:

    - [Fiber.current] uses {{!Picos_tls} [TLS]} to store the current fiber.
    - [Fiber.spawn] creates a new [Thread] for each fiber.
    - [Fiber.yield] just calls [Thread.yield].
    - [Computation.cancel_after] uses {{!Picos_domain.DLS} [DLS]} to store a
      priority queue of timeouts and a per-domain {{!Picos_select} background
      timeout thread} that runs a [Unix.select] loop to cancel computations.
    - [Trigger.await] uses {{!Picos_tls} [TLS]} to store a [Mutex] and
      [Condition] to suspend the thread.

    The operations initialize their resources, per-thread, and per-domain state
    and {{!Picos_select} background timeout thread}, only when actually used.
    If the default {{!Picos.Computation.cancel_after} [cancel_after]} is not
    used, no background timeout thread will be created and no per-domain state
    will be used.  If none of the defaults are used, no per-thread state will be
    used.

    ⚠️ This implementation is probably suitable for simple applications that do
    not spawn a lot.  If an application uses a lot of short lived fibers, then a
    more sophisticated implementation using some sort of thread pool will likely
    perform significantly better. *)

val implementation : (module Picos.Implementation)
(** A basic implementation of the effects based operations of {!Picos} using
    [Thread]s and a [Unix.select] based IO event loop. *)
