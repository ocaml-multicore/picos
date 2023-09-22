type t = { exn : exn; bt : Printexc.raw_backtrace }

let[@inline] get exn =
  let bt = Printexc.get_raw_backtrace () in
  { exn; bt }

let empty_backtrace = Printexc.get_callstack 0

let[@inline] get_callstack n exn =
  let bt = if n <= 0 then empty_backtrace else Printexc.get_callstack n in
  { exn; bt }

let raise { exn; bt } = Printexc.raise_with_backtrace exn bt
let discontinue k { exn; bt } = Effect.Deep.discontinue_with_backtrace k exn bt

let discontinue_with k { exn; bt } handler =
  Effect.Shallow.discontinue_with_backtrace k exn bt handler
