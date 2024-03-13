include Picos_private

module Computation = struct
  include Computation

  let block t =
    let trigger = Trigger.create () in
    if try_attach t trigger then begin
      match Trigger.await trigger with
      | None -> t
      | Some exn_bt ->
          detach t trigger;
          Exn_bt.raise exn_bt
    end
    else t

  let rec await t =
    match Atomic.get t with
    | Returned value -> value
    | Canceled exn_bt -> Exn_bt.raise exn_bt
    | Continue _ -> await (block t)
end
