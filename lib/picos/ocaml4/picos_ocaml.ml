open Picos_bootstrap

let[@inline never] error _ =
  raise (Sys_error "Picos.Handler.using not called for current thread")

module Handler = struct
  let default =
    let current = error
    and spawn _ _ = error
    and yield = error
    and cancel_after _ _ ~seconds:_ = error
    and await _ = error in
    Handler.Packed
      { context = (); handler = { current; spawn; yield; cancel_after; await } }
end

module Trigger = struct end
module Fiber = struct end
module Computation = struct end
