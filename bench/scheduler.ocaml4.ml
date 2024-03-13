let () = Picos.set_picos_implementation Picos_threaded.implementation
let run action = action ()
