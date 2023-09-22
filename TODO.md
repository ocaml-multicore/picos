# TODO

- Review cancelation in default handlers

- Change example scheduler to give priority to canceled fibers

- Add description of what the handlers must do

  - `Await`
  - `Cancel_after`
  - `Current`
  - `Yield`
  - `Spawn`

- Should cancelation exception be raised only once?

- Test what performance of `Fiber.current` could be with TLS

- Support OCaml 4

- Redesign `forbid` and `permit` to be safe with respect to async exceptions

- More tests

---

- Synchronous `Channel`

- `Bundle.fork : Bundle.t -> (unit -> unit) -> unit`

- Different Mutexes

  - `Unfair`
  - `Unchecked`

---

- Get feedback on the default behaviors
