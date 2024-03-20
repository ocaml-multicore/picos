# TODO

- More tests

- Consider having an additional "scheduler" or "runner" or "pool" concept and
  make it so that one can explicitly spawn fibers to run on a specific
  "scheduler".

- Consider interop between Picos and Lwt / Async in detail

- JavaScript scheduler

- Should cancelation exception be raised only once?

- Test what performance of `Fiber.current` could be with TLS

- Redesign `forbid` and `permit` to be safe with respect to async exceptions

---

- Wait for multiple (should be straightforward - promise example?)

- Synchronous `Channel`

- `Bundle.fork : Bundle.t -> (unit -> unit) -> unit`

- Different Mutexes

  - `Unfair`
  - `Unchecked`
