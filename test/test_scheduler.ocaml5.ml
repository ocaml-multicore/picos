let use_randos = Random.bool (Random.self_init ())

let run ?forbid main =
  if use_randos then Picos_randos.run ?forbid main
  else Picos_fifos.run ?forbid main
