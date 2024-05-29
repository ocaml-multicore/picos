[API reference](https://ocaml-multicore.github.io/picos/doc/picos/index.html)
&middot;
[Benchmarks](https://bench.ci.dev/ocaml-multicore/picos/branch/main?worker=pascal&image=bench.Dockerfile)
&middot;
[Stdlib Benchmarks](https://bench.ci.dev/ocaml-multicore/multicore-bench/branch/main?worker=pascal&image=bench.Dockerfile)

# **Picos** &mdash; Interoperable effects based concurrency

<p align="center"><a href="https://ocaml-multicore.github.io/picos/doc/picos/Picos/index.html"><img width="60%" src="https://raw.githubusercontent.com/ocaml-multicore/picos/main/doc/picos.svg"></a></p>

Picos is a
[systems programming](https://en.wikipedia.org/wiki/Systems_programming)
interface between effects based schedulers and concurrent abstractions. Picos is
designed to enable an ecosystem of
[interoperable](https://en.wikipedia.org/wiki/Interoperability) elements of
effects based cooperative concurrent programming models such as

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
  [asynchronous IO](https://en.wikipedia.org/wiki/Asynchronous_I/O) systems.

The `picos` package is divided into many small(er) libraries. The core is a
small library,
[`picos`](https://ocaml-multicore.github.io/picos/doc/picos/Picos/index.html),
that defines the essential scheduler interface. The rest of the libraries are
either sample schedulers (e.g.
[`picos.fifos`](https://ocaml-multicore.github.io/picos/doc/picos/Picos_fifos/index.html),
[`picos.lwt`](https://ocaml-multicore.github.io/picos/doc/picos/Picos_lwt/index.html),
[`picos.randos`](https://ocaml-multicore.github.io/picos/doc/picos/Picos_randos/index.html),
and
[`picos.threaded`](https://ocaml-multicore.github.io/picos/doc/picos/Picos_threaded/index.html)),
scheduler agnostic libraries (e.g.
[`picos.structured`](https://ocaml-multicore.github.io/picos/doc/picos/Picos_structured/index.html),
[`picos.sync`](https://ocaml-multicore.github.io/picos/doc/picos/Picos_sync/index.html),
[`picos.stdio`](https://ocaml-multicore.github.io/picos/doc/picos/Picos_stdio/index.html),
and
[`picos.select`](https://ocaml-multicore.github.io/picos/doc/picos/Picos_select/index.html)),
or auxiliary libraries.

⚠️ Please note that Picos is still considered experimental and unstable.

### Why?

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
to

1. make all of the previously mentioned libraries
   [Picos compatible](https://ocaml-multicore.github.io/picos/doc/picos/index.html#picos-compatible),
   i.e. to handle the Picos effects, and
2. have their elements
   [implemented in Picos](https://ocaml-multicore.github.io/picos/doc/picos/index.html#implemented-in-picos),
   i.e. to make them usable on other Picos-compatible schedulers.

Please read
[the reference manual](https://ocaml-multicore.github.io/picos/doc/picos/index.html)
for further information.
