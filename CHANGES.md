## 0.6.0

- Added a futex-like `Awaitable` abstraction as the `picos_std.awaitable`
  library (@polytypic)
- Changed the core Picos library to be internally built from a single `.ml` file
  (@polytypic)
- Optimized heap and stack usage of fibers and resource cleanup mechanisms and
  added workarounds for compiler generated space leaks due to closures
  (@polytypic)
- Added `lastly` as a safe alternative to `Fun.protect` (@polytypic)
- Workarounds for the `Uri` library not being threadsafe (@polytypic)
- Fixed to raise proper error when `Picos_io_select` has not been configured
  properly (@polytypic)
- Forbid cancelation propagation during `release` calls in the
  `picos_std.finally` library (@polytypic)
  - This is a change in behaviour and could be seen as a breaking change, but it
    should really be considered a bug fix.
- Renamed `(Ivar|Stream).poison` to `(Ivar|Stream).poison_at` and added
  `(Ivar|Stream).poison` with optional `?callstack:int` (@polytypic)

## 0.5.0

- Major additions, changes, bug fixes, improvements, and restructuring
  (@polytypic, @c-cube)

  - Additions:

    - Minimalistic Cohttp implementation
    - Implicitly propagated `Flock` of fibers for structured concurrency
    - Option to terminate `Bundle` and `Flock` on return
    - `Event` abstraction
    - Synchronization and communication primitives:
      - Incremental variable or `Ivar`
      - Countdown `Latch`
      - `Semaphore`
      - `Stream` of events
    - Multi-producer, multi-consumer lock-free queue optimized for schedulers
    - Multithreaded (work-stealing) FIFO scheduler
    - Support `quota` for FIFO based schedulers
    - Transactional interface for atomically completing multiple `Computation`s

  - Changes:

    - Redesigned resource management based on `('r -> 'a) -> 'a` functions
    - Redesigned `spawn` interface allowing `FLS` entries to be populated before
      spawn
    - Introduced concept of fatal errors, which must terminate the scheduler or
      the whole program
    - Simplified `FLS` interface
    - Removed `Exn_bt`

  - Improvements:

    - Signficantly reduced per fiber memory usage of various sample schedulers

  - Picos has now been split into multiple packages and libraries:

    - pkg: `picos`
      - lib: `picos`
      - lib: `picos.domain`
      - lib: `picos.thread`
    - pkg: `picos_aux`
      - lib: `picos_aux.htbl`
      - lib: `picos_aux.mpmcq`
      - lib: `picos_aux.mpscq`
      - lib: `picos_aux.rc`
    - pkg: `picos_lwt`
      - lib: `picos_lwt`
      - lib: `picos_lwt.unix`
    - pkg: `picos_meta` (integration tests)
    - pkg: `picos_mux`
      - lib: `picos_mux.fifo`
      - lib: `picos_mux.multififo`
      - lib: `picos_mux.random`
      - lib: `picos_mux.thread`
    - pkg: `picos_std`
      - lib: `picos_std.event`
      - lib: `picos_std.finally`
      - lib: `picos_std.structured`
      - lib: `picos_std.sync`
    - pkg: `picos_io`
      - lib: `picos_io`
      - lib: `picos_io.fd`
      - lib: `picos_io.select`
    - pkg: `picos_io_cohttp`
      - lib: `picos_io_cohttp`

## 0.4.0

- Renamed `Picos_mpsc_queue` to `Picos_mpscq`. (@polytypic)

- Core API changes:

  - Added `Computation.returned`. (@polytypic)

- `Lwt` interop improvements:

  - Fixed `Picos_lwt` handling of `Cancel_after` to not raise in case of
    cancelation. (@polytypic)

  - Redesigned `Picos_lwt` to take a `System` module, which must implement a
    semi thread-safe trigger mechanism to allow unblocking `Lwt` promises on the
    main thread. (@polytypic)

  - Added `Picos_lwt_unix` interface to `Lwt`, which includes an internal
    `System` module implemented using `Lwt_unix`. (@polytypic)

  - Dropped thunking from `Picos_lwt.await`. (@polytypic)

- Added a randomized multicore scheduler `Picos_randos` for testing.
  (@polytypic)

- Changed `Picos_select.check_configured` to always (re)configure signal
  handling on the current thread. (@polytypic)

- `Picos_structured`:

  - Added a minimalistic `Promise` abstraction. (@polytypic)
  - Changed to more consistently not treat `Terminate` as an error. (@polytypic)

- Changed schedulers to take `~forbid` as an optional argument. (@polytypic)

- Various minor additions, fixes, and documentation improvements. (@polytypic)

## 0.3.0

- Core API changes:

  - Added `Fiber.set_computation`, which represents a semantic change
  - Renamed `Fiber.computation` to `Fiber.get_computation`
  - Added `Computation.attach_canceler`
  - Added `Fiber.sleep`
  - Added `Fiber.create_packed`
  - Removed `Fiber.try_attach`
  - Removed `Fiber.detach`

  Most of the above changes were motivated by work on and requirements of the
  added structured concurrency library (@polytypic)

- Added a basic user level structured concurrent programming library
  `Picos_structured` (@polytypic)

- Added a functorized `Picos_lwt` providing direct style effects based interface
  to programming with Lwt (@polytypic)

- Added missing `Picos_stdio.Unix.select` (@polytypic)

## 0.2.0

- Documentation fixes and restructuring (@polytypic)
- Scheduler friendly `waitpid`, `wait`, and `system` in `Picos_stdio.Unix` for
  platforms other than Windows (@polytypic)
- Added `Picos_select.configure` to allow, and sometimes require, configuring
  `Picos_select` for co-operation with libraries that also deal with signals
  (@polytypic)
- Moved `Picos_tls` into `Picos_thread.TLS` (@polytypic)
- Enhanced `sleep` and `sleepf` in `Picos_stdio.Unix` to block in a scheduler
  friendly manner (@polytypic)

## 0.1.0

- First experimental release of Picos.

  Core:

  - `picos` — A framework for interoperable effects based concurrency.

  Sample schedulers:

  - `picos.fifos` — Basic single-threaded effects based Picos compatible
    scheduler for OCaml 5.
  - `picos.threaded` — Basic `Thread` based Picos compatible scheduler for
    OCaml 4.

  Scheduler agnostic libraries:

  - `picos.sync` — Basic communication and synchronization primitives for Picos.
  - `picos.stdio` — Basic IO facilities based on OCaml standard libraries for
    Picos.
  - `picos.select` — Basic `Unix.select` based IO event loop for Picos.

  Auxiliary libraries:

  - `picos.domain` — Minimalistic domain API available both on OCaml 5 and on
    OCaml 4.
  - `picos.exn_bt` — Wrapper for exceptions with backtraces.
  - `picos.fd` — Externally reference counted file descriptors.
  - `picos.htbl` — Lock-free hash table.
  - `picos.mpsc_queue` — Multi-producer, single-consumer queue.
  - `picos.rc` — External reference counting tables for disposable resources.
  - `picos.tls` — Thread-local storage.
