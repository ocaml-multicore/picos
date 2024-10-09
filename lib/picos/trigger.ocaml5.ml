type _ Effect.t += Await : t -> (exn * Printexc.raw_backtrace) option Effect.t

let await t = if is_initial t then Effect.perform (Await t) else None
