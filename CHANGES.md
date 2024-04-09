## Next version

- Enhanced `sleep` and `sleepf` in `Picos_stdio.Unix` to block in a scheduler
  friendly manner (@polytypic).

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
