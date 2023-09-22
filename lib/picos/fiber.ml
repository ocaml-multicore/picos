include Bootstrap.Fiber

type as_async = [ `Async ] t

let[@inline] equal t1 t2 = t1 == t2
let computation (Fiber r) = Computation.Packed r.computation
let check (Fiber r) = if not r.forbid then Computation.check r.computation

open struct
  let explicitly ~forbid (Fiber r) body =
    if r.forbid = forbid then body ()
    else
      match body (r.forbid <- forbid) with
      | value ->
          r.forbid <- not forbid;
          value
      | exception exn ->
          r.forbid <- not forbid;
          raise exn
end

let forbid t body = explicitly ~forbid:true t body
let permit t body = explicitly ~forbid:false t body

module FLS = struct
  type ('a, _) key = { index : int; default : Obj.t; compute : unit -> 'a }
  type 'a as_read_only = ('a, [ `Get ]) key

  open struct
    let compute () = failwith "impossible"
    let counter = Atomic.make 0
    let[@inline] unique () = Obj.repr counter

    let grow old_fls i =
      let new_length = Bits.ceil_pow_2_minus_1 (i + 1) in
      let new_fls = Array.make new_length (unique ()) in
      Array.blit old_fls 0 new_fls 0 (Array.length old_fls);
      new_fls
  end

  type 'a initial = Constant of 'a | Computed of (unit -> 'a)

  let new_key initial =
    let index = Atomic.fetch_and_add counter 1 in
    match initial with
    | Constant default -> { index; default = Obj.repr default; compute }
    | Computed compute -> { index; default = unique (); compute }

  let get (Fiber r) key =
    let fls = r.fls in
    if key.index < Array.length fls then begin
      let value = Array.unsafe_get fls key.index in
      if value != unique () then Obj.magic value
      else
        let value = key.default in
        if value != unique () then begin
          (* As the [fls] array was already large enough, we cache the default
             value in the array. *)
          Array.unsafe_set fls key.index (Sys.opaque_identity (Obj.repr value));
          Obj.magic value
        end
        else
          let value = key.compute () in
          Array.unsafe_set fls key.index (Sys.opaque_identity (Obj.repr value));
          value
    end
    else
      let value = key.default in
      if value != unique () then Obj.magic value
      else
        let value = key.compute () in
        let fls = grow fls key.index in
        r.fls <- fls;
        Array.unsafe_set fls key.index (Sys.opaque_identity (Obj.repr value));
        value

  let set (Fiber r) key value =
    let fls = r.fls in
    if key.index < Array.length fls then
      Array.unsafe_set fls key.index (Sys.opaque_identity (Obj.repr value))
    else
      let fls = grow fls key.index in
      r.fls <- fls;
      Array.unsafe_set fls key.index (Sys.opaque_identity (Obj.repr value))
end

type _ Effect.t += Current : [ `Sync | `Async ] t Effect.t

open struct
  let current_default () =
    let fiber = (Per_thread.get ()).fiber in
    check fiber;
    fiber
end

let current () =
  try Effect.perform Current
  with Effect.Unhandled Current -> current_default ()

type _ Effect.t +=
  | Spawn : {
      forbid : bool;
      computation : 'a Computation.as_cancelable;
      mains : (unit -> unit) list;
    }
      -> unit Effect.t

open struct
  let spawn_default forbid computation mains =
    let _ = current () in
    mains
    |> List.iter @@ fun main ->
       Systhreads.create
         (fun () -> main (Per_thread.set (create ~forbid computation)))
         ()
       |> ignore
end

let spawn ~forbid computation mains =
  try Effect.perform @@ Spawn { forbid; computation :> _; mains }
  with Effect.Unhandled (Spawn { forbid; computation; mains }) ->
    spawn_default forbid computation mains

type _ Effect.t += Yield : unit Effect.t

open struct
  let yield_default () =
    let _ = current () in
    Systhreads.yield ()
end

let yield () =
  try Effect.perform Yield with Effect.Unhandled Yield -> yield_default ()
