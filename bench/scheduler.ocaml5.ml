open Schedulers

let run action = Fifos.run ~forbid:false action
