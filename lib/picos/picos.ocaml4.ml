module TLS = TLS
module DLS = Domain.DLS
module Exn_bt = Exn_bt

module Trigger = struct
  include Bootstrap.Trigger

  let await t =
    match Atomic.get t with
    | Initial -> Default.await t
    | Signaled -> None
    | Awaiting _ -> awaiting ()
end

module Fiber = struct
  include Bootstrap.Fiber

  let current = Default.current

  let spawn ~forbid computation mains =
    let _ = current () in
    Default.spawn forbid computation mains

  let yield () =
    let _ = current () in
    Default.yield ()
end

module Computation = struct
  include Bootstrap.Computation

  let rec await t =
    match Atomic.get t with
    | Returned value -> value
    | Canceled exn_bt -> Exn_bt.raise exn_bt
    | Continue _ ->
        let trigger = Trigger.create () in
        if try_attach t trigger then begin
          match Trigger.await trigger with
          | None -> await t
          | Some exn_bt ->
              detach t trigger;
              Exn_bt.raise exn_bt
        end
        else await t

  let cancel_after computation ~seconds exn_bt =
    if seconds < 0.0 then invalid_arg "Computation: negative seconds"
    else
      let _ = Fiber.current () in
      Default.cancel_after seconds exn_bt computation
end
