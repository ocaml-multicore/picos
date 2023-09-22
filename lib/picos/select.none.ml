type file_descr

let stdin : file_descr = Obj.magic ()
let close _ = raise (Sys_error "Computation: cancel_after unavailable")
let read _ _ _ _ = close ()
let write _ _ _ _ = close ()
let pipe ?cloexec:_ _ = close ()
let select _ _ _ _ = close ()
