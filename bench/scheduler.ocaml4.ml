open Picos

let run action =
  Handler.using Picos_threaded.handler
    (Picos_threaded.create ~forbid:false (Computation.create ()))
    action
