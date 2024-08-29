include Common

let[@inline always] get exn =
  let bt = Printexc.get_raw_backtrace () in
  { exn; bt }

let empty_backtrace = Printexc.get_callstack 0

let[@inline always] get_callstack n exn =
  let bt = if n <= 0 then empty_backtrace else Printexc.get_callstack n in
  { exn; bt }

let raise t = Printexc.raise_with_backtrace t.exn t.bt

(* *)

let discontinue k t = Effect.Deep.discontinue_with_backtrace k t.exn t.bt

let discontinue_with k t handler =
  Effect.Shallow.discontinue_with_backtrace k t.exn t.bt handler
