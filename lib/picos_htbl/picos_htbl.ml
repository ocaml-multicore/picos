type 'k hashed_type = (module Stdlib.Hashtbl.HashedType with type t = 'k)

type ('k, 'v, _) tdt =
  | Nil : ('k, 'v, [> `Nil ]) tdt
  | Cons : {
      key : 'k;
      value : 'v;
      rest : ('k, 'v, [ `Nil | `Cons ]) tdt;
    }
      -> ('k, 'v, [> `Cons ]) tdt
  | Resize : {
      spine : ('k, 'v, [ `Nil | `Cons ]) tdt;
    }
      -> ('k, 'v, [> `Resize ]) tdt
      (** During resizing and snapshotting target buckets will be initialized
          with a physically unique [Resize] value and the source buckets will
          then be gradually updated to [Resize] values and the target buckets
          updated with data from the source buckets. *)

type ('k, 'v) bucket =
  | B : ('k, 'v, [< `Nil | `Cons | `Resize ]) tdt -> ('k, 'v) bucket
[@@unboxed]

type ('k, 'v) pending =
  | Nothing
  | Resize of { buckets : ('k, 'v) bucket Atomic.t array }

type ('k, 'v) state = {
  hash : 'k -> int;
  buckets : ('k, 'v) bucket Atomic.t array;
  equal : 'k -> 'k -> bool;
  non_linearizable_size : int Atomic.t array;
  pending : ('k, 'v) pending;
}

type ('k, 'v) t = ('k, 'v) state Atomic.t

let min_buckets = 16

let max_buckets =
  let n = Sys.max_array_length lsr 1 in
  let n = n lor (n lsr 1) in
  let n = n lor (n lsr 2) in
  let n = n lor (n lsr 4) in
  let n = n lor (n lsr 8) in
  let n = n lor (n lsr 16) in
  let n = if Sys.int_size <= 32 then n else n lor (n lsr 32) in
  let n = n + 1 in
  Int.min n (1 lsl 30 (* Limit of [hash] *))

let create (type k) ?hashed_type () =
  let equal, hash =
    match hashed_type with
    | None -> (( = ), Stdlib.Hashtbl.hash)
    | Some ((module Hashed_type) : k hashed_type) ->
        (Hashed_type.equal, Hashed_type.hash)
  in
  let buckets = Array.init min_buckets @@ fun _ -> Atomic.make (B Nil) in
  let non_linearizable_size =
    [| Atomic.make 0 |> Multicore_magic.copy_as_padded |]
  in
  let pending = Nothing in
  { hash; buckets; equal; non_linearizable_size; pending }
  |> Multicore_magic.copy_as_padded |> Atomic.make
  |> Multicore_magic.copy_as_padded

(* *)

let[@tail_mod_cons] rec filter t msk chk = function
  | Nil -> Nil
  | Cons r ->
      if t r.key land msk = chk then
        Cons { r with rest = filter t msk chk r.rest }
      else filter t msk chk r.rest

let split_hi r target i t spine =
  let high = Array.length r.buckets in
  let b = Array.unsafe_get target (i + high) in
  match Atomic.get b with
  | B (Resize _ as before) ->
      (* The [before] value is physically different for each resize and so
         checking that the resize has not finished is sufficient to ensure that
         the [compare_and_set] below does not disrupt the next resize. *)
      if Atomic.get t == r then
        let ((Nil | Cons _) as after) = filter r.hash high high spine in
        Atomic.compare_and_set b (B before) (B after) |> ignore
  | B (Nil | Cons _) -> ()

let split_lo r target i t spine =
  let b = Array.unsafe_get target i in
  match Atomic.get b with
  | B (Resize _ as before) ->
      (* The [before] value is physically different for each resize and so
         checking that the resize has not finished is sufficient to ensure that
         the [compare_and_set] below does not disrupt the next resize. *)
      if Atomic.get t == r then begin
        let ((Nil | Cons _) as after) =
          filter r.hash (Array.length r.buckets) 0 spine
        in
        Atomic.compare_and_set b (B before) (B after) |> ignore;
        split_hi r target i t spine
      end
  | B (Nil | Cons _) -> split_hi r target i t spine

let rec split_at r target i t backoff =
  let b = Array.unsafe_get r.buckets i in
  match Atomic.get b with
  | B ((Nil | Cons _) as spine) ->
      if Atomic.compare_and_set b (B spine) (B (Resize { spine })) then
        split_lo r target i t spine
      else split_at r target i t (Backoff.once backoff)
  | B (Resize spine_r) -> split_lo r target i t spine_r.spine

let rec split_all r target i t step =
  Atomic.get t == r
  &&
  let i = (i + step) land (Array.length r.buckets - 1) in
  split_at r target i t Backoff.default;
  i = 0 || split_all r target i t step

(* *)

let[@tail_mod_cons] rec merge rest = function
  | Nil -> rest
  | Cons r -> Cons { r with rest = merge rest r.rest }

let merge_at r target i t spine_lo spine_hi =
  let b = Array.unsafe_get target i in
  match Atomic.get b with
  | B (Resize _ as before) ->
      (* The [before] value is physically different for each resize and so
         checking that the resize has not finished is sufficient to ensure that
         the [compare_and_set] below does not disrupt the next resize. *)
      if Atomic.get t == r then
        let ((Nil | Cons _) as after) = merge spine_lo spine_hi in
        Atomic.compare_and_set b (B before) (B after) |> ignore
  | B (Nil | Cons _) -> ()

let rec merge_hi r target i t spine_lo backoff =
  let b = Array.unsafe_get r.buckets (i + Array.length target) in
  match Atomic.get b with
  | B ((Nil | Cons _) as spine) ->
      if Atomic.compare_and_set b (B spine) (B (Resize { spine })) then
        merge_at r target i t spine_lo spine
      else merge_hi r target i t spine_lo (Backoff.once backoff)
  | B (Resize spine_r) -> merge_at r target i t spine_lo spine_r.spine

let rec merge_lo r target i t backoff =
  let b = Array.unsafe_get r.buckets i in
  match Atomic.get b with
  | B ((Nil | Cons _) as spine) ->
      if Atomic.compare_and_set b (B spine) (B (Resize { spine })) then
        merge_hi r target i t spine Backoff.default
      else merge_lo r target i t (Backoff.once backoff)
  | B (Resize spine_r) -> merge_hi r target i t spine_r.spine Backoff.default

let rec merge_all r target i t step =
  Atomic.get t == r
  &&
  let i = (i + step) land (Array.length target - 1) in
  merge_lo r target i t Backoff.default;
  i = 0 || merge_all r target i t step

(* *)

let copy_to r target i t
    ((Nil | Cons _) as spine : (_, _, [ `Nil | `Cons ]) tdt) =
  let b = Array.unsafe_get target i in
  match Atomic.get b with
  | B (Resize _ as before) ->
      (* The [before] value is physically different for each resize and so
         checking that the resize has not finished is sufficient to ensure that
         the [compare_and_set] below does not disrupt the next resize. *)
      if Atomic.get t == r then
        Atomic.compare_and_set b (B before) (B spine) |> ignore
  | B (Nil | Cons _) -> ()

let rec copy_at r target i t backoff =
  let b = Array.unsafe_get r.buckets i in
  match Atomic.get b with
  | B ((Nil | Cons _) as spine) ->
      if Atomic.compare_and_set b (B spine) (B (Resize { spine })) then
        copy_to r target i t spine
      else copy_at r target i t (Backoff.once backoff)
  | B (Resize spine_r) -> copy_to r target i t spine_r.spine

let rec copy_all r target i t step =
  Atomic.get t == r
  &&
  let i = (i + step) land (Array.length target - 1) in
  copy_at r target i t Backoff.default;
  i = 0 || copy_all r target i t step

(* *)

let[@inline never] rec finish t r =
  match r.pending with
  | Nothing -> r
  | Resize { buckets } ->
      let high_source = Array.length r.buckets in
      let high_target = Array.length buckets in
      (* We step by random amount to better allow cores to work in parallel.
         The number of buckets is always a power of two, so any odd number is
         relatively prime or coprime. *)
      let step = Random.bits () lor 1 in
      if
        if high_source < high_target then begin
          (* We are growing the table. *)
          split_all r buckets 0 t step
        end
        else if high_target < high_source then begin
          (* We are shrinking the table. *)
          merge_all r buckets 0 t step
        end
        else begin
          (* We are snaphotting the table. *)
          copy_all r buckets 0 t step
        end
      then
        let new_r =
          { r with buckets; pending = Nothing }
          |> Multicore_magic.copy_as_padded
        in
        if Atomic.compare_and_set t r new_r then new_r
        else finish t (Atomic.get t)
      else finish t (Atomic.get t)

(* *)

let rec estimated_size cs n sum =
  let n = n - 1 in
  if 0 <= n then estimated_size cs n (sum + Atomic.get (Array.unsafe_get cs n))
  else sum

(** This only gives an "estimate" of the size, which can be off by one or more
    and even be negative, so this must be used with care. *)
let estimated_size r =
  let cs = r.non_linearizable_size in
  let n = Array.length cs - 1 in
  estimated_size cs n (Atomic.get (Array.unsafe_get cs n))

let[@inline never] try_resize t r new_capacity =
  (* We must make sure that on every resize we use a physically different
     [Resize _] value to indicate unprocessed target buckets.  The use of
     [Sys.opaque_identity] below ensures that a new value is allocated. *)
  let resize_avoid_aba = B (Resize { spine = Sys.opaque_identity Nil }) in
  let buckets =
    Array.init new_capacity @@ fun _ -> Atomic.make resize_avoid_aba
  in
  let new_r = { r with pending = Resize { buckets } } in
  Atomic.compare_and_set t r new_r
  && begin
       finish t new_r |> ignore;
       true
     end

let rec adjust_estimated_size t r mask delta result =
  let i = Multicore_magic.instantaneous_domain_index () in
  let n = Array.length r.non_linearizable_size in
  if i < n then begin
    Atomic.fetch_and_add (Array.unsafe_get r.non_linearizable_size i) delta
    |> ignore;
    (* Reading the size is potentially expensive, so we only check it
       occasionally.  The bigger the table the less frequently we should need to
       resize. *)
    if Random.bits () land mask = 0 && Atomic.get t == r then begin
      let estimated_size = estimated_size r in
      let capacity = Array.length r.buckets in
      if capacity < estimated_size && capacity < max_buckets then
        try_resize t r (capacity + capacity) |> ignore
      else if
        min_buckets < capacity
        && estimated_size + estimated_size + estimated_size < capacity
      then try_resize t r (capacity lsr 1) |> ignore
    end;
    result
  end
  else
    let new_cs =
      (* We use [n + n + 1] as it keeps the length of the array as a power of 2
         minus 1 and so the size of the array/block including header word will
         be a power of 2. *)
      Array.init (n + n + 1) @@ fun i ->
      if i < n then Array.unsafe_get r.non_linearizable_size i
      else Atomic.make 0 |> Multicore_magic.copy_as_padded
    in
    let new_r =
      { r with non_linearizable_size = new_cs }
      |> Multicore_magic.copy_as_padded
    in
    let r = if Atomic.compare_and_set t r new_r then new_r else Atomic.get t in
    adjust_estimated_size t r mask delta result

(* *)

(** [get] only returns with a state where [pending = Nothing]. *)
let[@inline] get t =
  let r = Atomic.get t in
  if r.pending == Nothing then r else finish t r

(* *)

let rec assoc_node t key = function
  | Nil -> (Nil : (_, _, [< `Nil | `Cons ]) tdt)
  | Cons r as cons -> if t r.key key then cons else assoc_node t key r.rest

let find_node t key =
  (* Reads can proceed in parallel with writes. *)
  let r = Atomic.get t in
  let h = r.hash key in
  let mask = Array.length r.buckets - 1 in
  let i = h land mask in
  match Atomic.get (Array.unsafe_get r.buckets i) with
  | B Nil -> Nil
  | B (Cons cons_r as cons) ->
      if r.equal cons_r.key key then cons
      else assoc_node r.equal key cons_r.rest
  | B (Resize resize_r) ->
      (* A resize is in progress.  The spine of the resize still holds what was
         in the bucket before resize reached that bucket. *)
      assoc_node r.equal key resize_r.spine

(* *)

let find_exn t key =
  match find_node t key with
  | Nil -> raise_notrace Not_found
  | Cons r -> r.value

let mem t key = find_node t key != Nil

(* *)

let rec try_add t key value backoff =
  let r = get t in
  let h = r.hash key in
  let mask = Array.length r.buckets - 1 in
  let i = h land mask in
  let b = Array.unsafe_get r.buckets i in
  match Atomic.get b with
  | B Nil ->
      let after = Cons { key; value; rest = Nil } in
      if Atomic.compare_and_set b (B Nil) (B after) then
        adjust_estimated_size t r mask 1 true
      else try_add t key value (Backoff.once backoff)
  | B (Cons _ as before) ->
      if assoc_node r.equal key before != Nil then false
      else
        let after = Cons { key; value; rest = before } in
        if Atomic.compare_and_set b (B before) (B after) then
          adjust_estimated_size t r mask 1 true
        else try_add t key value (Backoff.once backoff)
  | B (Resize _) -> try_add t key value Backoff.default

let try_add t key value = try_add t key value Backoff.default

(* *)

let[@tail_mod_cons] rec dissoc t key = function
  | Nil -> raise_notrace Not_found
  | Cons r ->
      if t key r.key then r.rest else Cons { r with rest = dissoc t key r.rest }

let rec remove_node t key backoff =
  let r = get t in
  let h = r.hash key in
  let mask = Array.length r.buckets - 1 in
  let i = h land mask in
  let b = Array.unsafe_get r.buckets i in
  match Atomic.get b with
  | B Nil -> Nil
  | B (Cons cons_r as before) -> begin
      if r.equal cons_r.key key then
        if Atomic.compare_and_set b (B before) (B cons_r.rest) then
          adjust_estimated_size t r mask (-1) before
        else remove_node t key (Backoff.once backoff)
      else
        match dissoc r.equal key cons_r.rest with
        | (Nil | Cons _) as rest ->
            if
              Atomic.compare_and_set b (B before)
                (B (Cons { cons_r with rest }))
            then
              assoc_node r.equal key cons_r.rest
              |> adjust_estimated_size t r mask (-1)
            else remove_node t key (Backoff.once backoff)
        | exception Not_found -> Nil
    end
  | B (Resize _) -> remove_node t key Backoff.default

let try_remove t key = remove_node t key Backoff.default != Nil

let remove_exn t key =
  match remove_node t key Backoff.default with
  | Nil -> raise_notrace Not_found
  | Cons r -> r.value

(* *)

let rec to_seq t backoff =
  let r = get t in
  if try_resize t r (Array.length r.buckets) then begin
    (* At this point the resize has been completed and a new array is used for
       buckets and [r.buckets] now has an immutable copy of what was in the hash
       table. *)
    let snapshot = r.buckets in
    let rec loop i kvs () =
      match kvs with
      | Nil ->
          if i = Array.length snapshot then Seq.Nil
          else
            loop (i + 1)
              (match Atomic.get (Array.unsafe_get snapshot i) with
              | B (Resize spine_r) -> spine_r.spine
              | B (Nil | Cons _) ->
                  (* After resize only [Resize] values should be left in the old
                     buckets. *)
                  assert false)
              ()
      | Cons r -> Seq.Cons ((r.key, r.value), loop i r.rest)
    in
    loop 0 Nil
  end
  else to_seq t (Backoff.once backoff)

let to_seq t = to_seq t Backoff.default
