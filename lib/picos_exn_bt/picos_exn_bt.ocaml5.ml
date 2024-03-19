include Common

let discontinue k t = Effect.Deep.discontinue_with_backtrace k t.exn t.bt

let discontinue_with k t handler =
  Effect.Shallow.discontinue_with_backtrace k t.exn t.bt handler
