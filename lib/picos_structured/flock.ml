open Picos

let[@inline never] no_flock () = invalid_arg "no flock"

let get () =
  match Fiber.FLS.get (Fiber.current ()) Bundle.flock_key with
  | Nothing -> no_flock ()
  | Bundle _ as t -> t

let terminate_after ?callstack ~seconds () =
  Bundle.terminate_after ?callstack (get ()) ~seconds

let terminate ?callstack () = Bundle.terminate ?callstack (get ())
let error ?callstack exn_bt = Bundle.error (get ()) ?callstack exn_bt
let fork_as_promise thunk = Bundle.fork_as_promise_pass (get ()) thunk FLS
let fork action = Bundle.fork_pass (get ()) action FLS
let join_after fn = Bundle.join_after_pass fn Bundle.FLS
