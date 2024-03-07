type 'b t = {
  try_handle :
    'a. 'a Effect.t -> (('a, 'b) Effect.Deep.continuation -> 'b) option;
}
[@@unboxed]

val call :
  'b t option -> 'a Effect.t -> (('a, 'b) Effect.Deep.continuation -> 'b) option
