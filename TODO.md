# TODO

- Reconsider use of domain_shims. Does it do the right thing in this context?

- Change example scheduler to give priority to canceled fibers

- Should cancelation exception be raised only once?

- Test what performance of `Fiber.current` could be with TLS

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
