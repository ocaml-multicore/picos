let () = Random.self_init ()
let run ?forbid main = Picos_threaded.run ?forbid main
