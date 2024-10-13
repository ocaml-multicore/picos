let cancel_after computation ~seconds exn bt =
  if not (0.0 <= seconds) then error_negative_or_nan ();
  let (E r) = Handler.get () in
  r.handler.cancel_after r.context computation ~seconds exn bt
