open Picos

let get () = Bundle.get_flock (Fiber.current ())

let terminate_after ?callstack ~seconds () =
  Bundle.terminate_after ?callstack (get ()) ~seconds

let terminate ?callstack () = Bundle.terminate ?callstack (get ())
let error ?callstack exn_bt = Bundle.error (get ()) ?callstack exn_bt
let fork_as_promise thunk = Bundle.fork_as_promise_pass (get ()) thunk FLS
let fork action = Bundle.fork_pass (get ()) action FLS

let join_after ?callstack ?on_return fn =
  Bundle.join_after_pass ?callstack ?on_return fn Bundle.FLS
