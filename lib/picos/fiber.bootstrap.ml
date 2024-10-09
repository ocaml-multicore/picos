type non_float = [ `Non_float of non_float ]

type _ tdt =
  | Nothing : [> `Nothing ] tdt
  | Fiber : {
      mutable forbid : bool;
      mutable packed : Computation.packed;
      mutable fls : non_float array;
    }
      -> [> `Fiber ] tdt

type t = [ `Fiber ] tdt

let create_packed ~forbid packed = Fiber { forbid; packed; fls = [||] }

let create ~forbid computation =
  create_packed ~forbid (Computation.Packed computation)

let has_forbidden (Fiber r : t) = r.forbid

let is_canceled (Fiber r : t) =
  (not r.forbid)
  &&
  let (Packed computation) = r.packed in
  Computation.is_canceled computation

let canceled (Fiber r : t) =
  if r.forbid then None
  else
    let (Packed computation) = r.packed in
    Computation.canceled computation

let get_computation (Fiber r : t) = r.packed
let set_computation (Fiber r : t) packed = r.packed <- packed

let check (Fiber r : t) =
  if not r.forbid then
    let (Packed computation) = r.packed in
    Computation.check computation

let[@inline] equal t1 t2 = t1 == t2

let exchange (Fiber r : t) ~forbid =
  let before = r.forbid in
  r.forbid <- forbid;
  before

let set (Fiber r : t) ~forbid = r.forbid <- forbid

let explicitly (Fiber r : t) body ~forbid =
  if r.forbid = forbid then body ()
  else
    match body (r.forbid <- forbid) with
    | value ->
        r.forbid <- not forbid;
        value
    | exception exn ->
        r.forbid <- not forbid;
        raise exn

let forbid t body = explicitly t body ~forbid:true
let permit t body = explicitly t body ~forbid:false

let try_suspend (Fiber r : t) trigger x y resume =
  let (Packed computation) = r.packed in
  if not r.forbid then begin
    if Computation.try_attach computation trigger then
      let success = Trigger.on_signal trigger x y resume in
      if success then success
      else begin
        Computation.detach computation trigger;
        false
      end
    else if Computation.is_canceled computation then begin
      Trigger.dispose trigger;
      false
    end
    else Trigger.on_signal trigger x y resume
  end
  else Trigger.on_signal trigger x y resume

let[@inline] unsuspend (Fiber r : t) trigger =
  assert (Trigger.is_signaled trigger);
  r.forbid
  ||
  let (Packed computation) = r.packed in
  Computation.unsafe_unsuspend computation Backoff.default

module FLS = struct
  type fiber = t
  type 'a t = int

  let counter = Atomic.make 0
  let unique = Sys.opaque_identity (Obj.magic counter : non_float)

  let grow old_fls i =
    let ceil_pow_2_minus_1 n =
      let n = Nativeint.of_int n in
      let n = Nativeint.logor n (Nativeint.shift_right_logical n 1) in
      let n = Nativeint.logor n (Nativeint.shift_right_logical n 2) in
      let n = Nativeint.logor n (Nativeint.shift_right_logical n 4) in
      let n = Nativeint.logor n (Nativeint.shift_right_logical n 8) in
      let n = Nativeint.logor n (Nativeint.shift_right_logical n 16) in
      Nativeint.to_int
        (if Sys.int_size > 32 then
           Nativeint.logor n (Nativeint.shift_right_logical n 32)
         else n)
    in
    let new_length = ceil_pow_2_minus_1 (i + 1) in
    let new_fls = Array.make new_length unique in
    Array.blit old_fls 0 new_fls 0 (Array.length old_fls);
    new_fls

  let create () = Atomic.fetch_and_add counter 1

  exception Not_set

  let get_exn (type a) (Fiber r : fiber) (key : a t) =
    if key < Array.length r.fls && unique != Array.unsafe_get r.fls key then
      Sys.opaque_identity (Obj.magic (Array.unsafe_get r.fls key) : a)
    else raise_notrace Not_set

  let get (type a) (Fiber r : fiber) (key : a t) ~default =
    if key < Array.length r.fls && unique != Array.unsafe_get r.fls key then
      Sys.opaque_identity (Obj.magic (Array.unsafe_get r.fls key) : a)
    else default

  let reserve (type a) (Fiber r : fiber) (key : a t) =
    let fls = r.fls in
    if Array.length fls <= key then r.fls <- grow fls key

  let set (type a) (Fiber r : fiber) (key : a t) (value : a) =
    let fls = r.fls in
    if key < Array.length fls then
      Array.unsafe_set fls key
        (Sys.opaque_identity (Obj.magic value : non_float))
    else
      let fls = grow fls key in
      r.fls <- fls;
      Array.unsafe_set fls key
        (Sys.opaque_identity (Obj.magic value : non_float))

  let remove (type a) (Fiber r : fiber) (key : a t) =
    let fls = r.fls in
    if key < Array.length fls then Array.unsafe_set fls key unique
end
