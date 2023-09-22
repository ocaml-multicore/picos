let yield = Fun.id

type t

let self : unit -> t = Obj.magic
let create _ _ = raise (Sys_error "Fiber: spawn unavailable")
let join = ignore
