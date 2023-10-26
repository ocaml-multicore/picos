# TODO

- Make sure that when `unix` or `threads.posix` is not available the default
  implementations report errors and do not put system in invalid state.

- Consider having an additional "scheduler" or "runner" or "pool" concept and
  make it so that one can explicitly spawn fibers to run on a specific
  "scheduler".

- Consider whether it would make sense to provide an approach to waiting for
  multiple things with a timeout. Maybe have it in a separate communication and
  synchronization primitives library rather than in Picos?

- Consider interop between Picos and Lwt / Async in detail

- JavaScript scheduler

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
  - Feedback: Does it use a pool for threads?
    - Conclusion: It would probably make sense to be able to replace the
      defaults on OCaml 4. It is a bit too much to implement pools in Picos?
