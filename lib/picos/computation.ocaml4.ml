let cancel_after computation ~seconds exn bt =
  check_non_negative seconds;
  let (E r) = Handler.get () in
  r.handler.cancel_after r.context computation ~seconds exn bt
