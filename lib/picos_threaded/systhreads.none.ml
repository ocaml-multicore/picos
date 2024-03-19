let yield : unit -> unit = Fun.id

type t

let self : unit -> t = Obj.magic

let create (type a b) (_ : a -> b) (_ : a) : t =
  raise (Sys_error "Fiber: spawn unavailable")

let join : t -> unit = ignore
