type 'a key = { index : int; compute : unit -> 'a }

open struct
  let counter = Atomic.make 0
  let unique () = Obj.repr counter

  type t = { _id : int; mutable tls : Obj.t }

  let[@inline never] grow_tls t before index =
    let new_length = Bits.ceil_pow_2_minus_1 (index + 1) in
    let after = Array.make new_length (unique ()) in
    Array.blit before 0 after 0 (Array.length before);
    t.tls <- Obj.repr after;
    after

  let[@inline] get_tls index =
    let t = Obj.magic (Thread.self ()) in
    let tls = t.tls in
    if Obj.is_int tls then grow_tls t [||] index
    else
      let tls = (Obj.magic tls : Obj.t array) in
      if index < Array.length tls then tls else grow_tls t tls index
end

let new_key compute =
  let index = Atomic.fetch_and_add counter 1 in
  { index; compute }

let get key =
  let tls = get_tls key.index in
  let value = Array.unsafe_get tls key.index in
  if value != unique () then Obj.magic value
  else
    let value = key.compute () in
    Array.unsafe_set tls key.index (Obj.repr (Sys.opaque_identity value));
    value

let set key value =
  let tls = get_tls key.index in
  Array.unsafe_set tls key.index (Obj.repr (Sys.opaque_identity value))
