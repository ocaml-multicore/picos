module Exn_bt = Picos_bootstrap.Exn_bt

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
  let wait t = if is_running t then ignore (block t)
end

module Fiber = struct
  include Picos_bootstrap.Fiber
  include Picos_ocaml.Fiber

  module Maybe = struct
    let[@inline never] not_a_fiber () = invalid_arg "not a fiber"

    type t = T : [< `Nothing | `Fiber ] tdt -> t [@@unboxed]

    let[@inline] to_fiber_or_current = function
      | T Nothing -> current ()
      | T (Fiber _ as t) -> t

    let[@inline] or_current t = T (to_fiber_or_current t)
    let nothing = T Nothing
    let[@inline] equal x y = x == y || x == nothing || y == nothing
    let[@inline] unequal x y = x != y || x == nothing
    let[@inline] of_fiber t = T t

    let[@inline] current_if checked =
      match checked with
      | None | Some true -> of_fiber (current ())
      | Some false -> nothing

    let[@inline] current_and_check_if checked =
      match checked with
      | None | Some true ->
          let fiber = current () in
          check fiber;
          of_fiber fiber
      | Some false -> nothing

    let[@inline] check = function
      | T Nothing -> ()
      | T (Fiber _ as t) -> check t

    let[@inline] to_fiber = function
      | T Nothing -> not_a_fiber ()
      | T (Fiber _ as t) -> t
  end

  exception Done

  let done_bt = Exn_bt.get_callstack 0 Done

  let sleep ~seconds =
    let sleep = Computation.create ~mode:`LIFO () in
    Computation.cancel_after ~seconds sleep done_bt;
    let trigger = Trigger.create () in
    if Computation.try_attach sleep trigger then
      match Trigger.await trigger with
      | None -> ()
      | Some exn_bt ->
          Computation.finish sleep;
          Exn_bt.raise exn_bt
end

module Handler = struct
  include Picos_bootstrap.Handler
  include Picos_ocaml.Handler
end
