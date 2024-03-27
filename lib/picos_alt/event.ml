type 'a t = {
  subscribe : int Picos.Computation.t -> int -> [ `ok | `already_happened ];
  resolve : unit -> 'a;
}

exception Cancel

let make ~subscribe ~resolve () : _ t = { subscribe; resolve }
let return_comp_ _trigger comp i = Picos.Computation.return comp i

let timeout ~seconds : unit t =
  let subscribe comp i =
    let trigger = Picos.Trigger.create () in
    let comp' = Picos.Computation.create () in
    let ok = Picos.Computation.try_attach comp' trigger in
    assert ok;
    let _ : bool =
      (Picos.Trigger.on_signal [@alert "-handler"]) trigger comp i return_comp_
    in
    Picos.Computation.cancel_after comp' ~seconds
      (Picos.Exn_bt.get_callstack 0 Exit);
    if Picos.Trigger.is_signaled trigger then `already_happened else `ok
  in

  { subscribe; resolve = ignore }
