let () = Random.self_init ()
let run ?max_domains:_ ?forbid main = Picos_threaded.run ?forbid main
