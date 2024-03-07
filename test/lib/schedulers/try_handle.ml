type 'b t = {
  try_handle :
    'a. 'a Effect.t -> (('a, 'b) Effect.Deep.continuation -> 'b) option;
}
[@@unboxed]

let call t e = match t with None -> None | Some r -> r.try_handle e
