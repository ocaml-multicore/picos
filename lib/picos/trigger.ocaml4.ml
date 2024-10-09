let await t =
  if is_initial t then
    let (E r) = Handler.get () in
    r.handler.await r.context t
  else None
