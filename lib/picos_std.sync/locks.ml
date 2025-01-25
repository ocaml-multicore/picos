let[@inline] holding t thunk ~acquire ~release ~poison =
  acquire t;
  match thunk () with
  | value ->
      release t;
      value
  | exception exn ->
      let bt = Printexc.get_raw_backtrace () in
      (* Lock is left locked. *)
      poison t;
      Printexc.raise_with_backtrace exn bt

let[@inline] protect t thunk ~acquire ~release =
  acquire t;
  match thunk () with
  | value ->
      release t;
      value
  | exception exn ->
      let bt = Printexc.get_raw_backtrace () in
      release t;
      Printexc.raise_with_backtrace exn bt
