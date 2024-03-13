module Exn_bt = Picos_exn_bt

module Trigger = struct
  include Picos_bootstrap.Trigger
  include Picos_ocaml.Trigger
end

module Computation = struct
  include Picos_bootstrap.Computation
  include Picos_ocaml.Computation

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

  let await t = get_or block t
end

module Fiber = struct
  include Picos_bootstrap.Fiber
  include Picos_ocaml.Fiber
end

module type Implementation = Picos_bootstrap.Implementation

let set_picos_implementation = Picos_ocaml.set_picos_implementation
