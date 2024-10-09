let current () =
  let (E r) = Handler.get () in
  r.handler.current r.context

let spawn fiber main =
  let (E r) = Handler.get () in
  r.handler.spawn r.context fiber main

let yield () =
  let (E r) = Handler.get () in
  r.handler.yield r.context
