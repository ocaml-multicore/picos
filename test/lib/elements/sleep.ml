open Picos

let exit_bt = Exn_bt.get_callstack 0 Exit

let sleepf seconds =
  let sleep = Computation.create () in
  Computation.cancel_after ~seconds sleep exit_bt;
  let trigger = Trigger.create () in
  if Computation.try_attach sleep trigger then
    match Trigger.await trigger with
    | None ->
        (* This means that the timeout was triggered and [sleep] has been
           canceled. *)
        ()
    | Some exn_bt ->
        (* This means that the underlying fiber was canceled.

           Note that the [exn_bt] does not come from [sleep]!

           We must finish the [sleep] computation, which signals the scheduler
           that the timeout is no longer needed. *)
        Computation.finish sleep;
        Exn_bt.raise exn_bt
