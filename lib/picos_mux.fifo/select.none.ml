let cancel_after _ ~seconds:_ _ =
  raise (Sys_error "Computation: cancel_after unavailable; install picos_io")

let check_configured = Fun.id
