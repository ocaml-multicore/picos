let block t =
  let trigger = Trigger.create () in
  if try_attach t trigger then begin
    match Trigger.await trigger with
    | None -> t
    | Some (exn, bt) ->
        detach t trigger;
        Printexc.raise_with_backtrace exn bt
  end
  else t

let await t = get_or block t
let wait t = if is_running t then ignore (block t)
