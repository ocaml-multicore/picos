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
