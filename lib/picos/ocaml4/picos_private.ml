module Exn_bt = Picos_exn_bt
module Default = Picos_default.Make (Picos_private_bootstrap)

module Trigger = struct
  include Picos_private_bootstrap.Trigger

  let await t =
    match Atomic.get t with
    | Initial -> Default.Trigger.await t
    | Signaled -> None
    | Awaiting _ -> awaiting ()
end

module Fiber = struct
  include Picos_private_bootstrap.Fiber

  let current = Default.Fiber.current

  let spawn ~forbid computation mains =
    let _ = current () in
    Default.Fiber.spawn ~forbid computation mains

  let yield () =
    let _ = current () in
    Default.Fiber.yield ()
end

module Computation = struct
  include Picos_private_bootstrap.Computation

  let cancel_after computation ~seconds exn_bt =
    if 0.0 <= seconds then
      let _ = Fiber.current () in
      Default.Computation.cancel_after computation ~seconds exn_bt
    else negative ()
end
