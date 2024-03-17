open Picos

let run main =
  Handler.using Picos_threaded.handler
    (Picos_threaded.create ~forbid:false (Computation.create ()))
    main
