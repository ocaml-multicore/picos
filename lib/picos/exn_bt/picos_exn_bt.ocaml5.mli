include Common.Intf

val discontinue : ('a, 'b) Effect.Deep.continuation -> t -> 'b
(** [discontinue k exn_bt] is equivalent to
    [Effect.Deep.discontinue_with_backtrace k exn_bt.exn exn_bt.bt]. *)

val discontinue_with :
  ('a, 'b) Effect.Shallow.continuation ->
  t ->
  ('b, 'c) Effect.Shallow.handler ->
  'c
(** [discontinue_with k exn_bt h] is equivalent to
    [Effect.Shallow.discontinue_with_backtrace k exn_bt.exn exn_bt.bt h]. *)
