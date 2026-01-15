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

module Atomic = Multicore_magic.Transparent_atomic
module Atomic_array = Multicore_magic.Atomic_array

type 'k hashed_type = (module Stdlib.Hashtbl.HashedType with type t = 'k)

type ('k, 'v, _) tdt =
  | Nil : ('k, 'v, [> `Nil ]) tdt
  | Cons : {
      key : 'k;
      value : 'v;
      rest : ('k, 'v, [ `Nil | `Cons ]) tdt;
    }
      -> ('k, 'v, [> `Cons ]) tdt
  | Fresh : ('k, 'v, [> `Fresh ]) tdt
  | Frozen : {
      mutable spine : ('k, 'v, [ `Nil | `Cons ]) tdt;
    }
      -> ('k, 'v, [> `Frozen ]) tdt

type ('k, 'v) bucket =
  | B : ('k, 'v, [< `Nil | `Cons | `Fresh | `Frozen ]) tdt -> ('k, 'v) bucket
[@@unboxed]

module Past : sig
  type ('k, 'v) t

  type ('k, 'v) table = {
    buckets : ('k, 'v) bucket Atomic_array.t;
    non_linearizable_size_delta : int Atomic.t array;
    size : int;
  }

  val of_size : int -> ('k, 'v) t
  val of_table : ('k, 'v) table -> ('k, 'v) t
  val is_size : ('k, 'v) t -> bool
  val unsafe_as_size : ('k, 'v) t -> int
  val unsafe_as_table : ('k, 'v) t -> ('k, 'v) table
end = struct
  type ('k, 'b) t = Obj.t

  type ('k, 'v) table = {
    buckets : ('k, 'v) bucket Atomic_array.t;
    non_linearizable_size_delta : int Atomic.t array;
    size : int;
  }

  let of_size = Obj.repr
  let of_table = Obj.repr
  let is_size = Obj.is_int
  let unsafe_as_size = Obj.obj
  let unsafe_as_table = Obj.obj
end

type ('k, 'v) state = {
  hash : 'k -> int;
  buckets : ('k, 'v) bucket Atomic_array.t;
  equal : 'k -> 'k -> bool;
  non_linearizable_size_delta : int Atomic.t array;
  mutable past : ('k, 'v) Past.t;
  min_buckets : int;
  max_buckets : int;
}

type ('k, 'v) t = ('k, 'v) state Atomic.t

(* *)

let lo_buckets = 1 lsl 3

and hi_buckets =
  let mask = ceil_pow_2_minus_1 Sys.max_array_length in
  mask lxor (mask lsr 1)

let min_buckets_default = 1 lsl 4
and max_buckets_default = Int.min hi_buckets (1 lsl 30)

let create (type k) ?hashed_type ?min_buckets ?max_buckets () =
  let min_buckets =
    match min_buckets with
    | None -> min_buckets_default
    | Some n ->
        let n = Int.max lo_buckets n |> Int.min hi_buckets in
        ceil_pow_2_minus_1 (n - 1) + 1
  in
  let max_buckets =
    match max_buckets with
    | None -> Int.max min_buckets max_buckets_default
    | Some n ->
        let n = Int.max min_buckets n |> Int.min hi_buckets in
        ceil_pow_2_minus_1 (n - 1) + 1
  in
  let equal, hash =
    match hashed_type with
    | None ->
        (( = ), Stdlib.Hashtbl.seeded_hash (Int64.to_int (Random.bits64 ())))
    | Some ((module Hashed_type) : k hashed_type) ->
        (Hashed_type.equal, Hashed_type.hash)
  in
  {
    hash;
    buckets = Atomic_array.make min_buckets (B Nil);
    equal;
    non_linearizable_size_delta =
      Array.init
        (ceil_pow_2_minus_1
           (Multicore_magic.instantaneous_domain_index () lor 1))
        (fun _ -> Atomic.make_contended 0);
    past = Past.of_size 0;
    min_buckets;
    max_buckets;
  }
  |> Atomic.make_contended

(* *)

let rec freeze i past_bs =
  match Atomic_array.unsafe_fenceless_get past_bs i with
  | B Fresh -> failwith "freeze"
  | B ((Nil | Cons _) as spine) ->
      if
        Atomic_array.unsafe_compare_and_set past_bs i (B spine)
          (B (Frozen { spine }))
      then spine
      else freeze i past_bs
  | B (Frozen spine_r) -> spine_r.spine

let set_if_fresh live_bs i past =
  if Atomic_array.unsafe_fenceless_get live_bs i == B Fresh then
    ignore
      (Atomic_array.unsafe_compare_and_set live_bs i (B Fresh) (B past) : bool)

let copy _s i live_bs past_bs =
  let spine = freeze i past_bs in
  set_if_fresh live_bs i spine

let rec split hash lo live_bs high past_lo past_hi = function
  | Nil ->
      set_if_fresh live_bs lo past_lo;
      set_if_fresh live_bs (lo + high) past_hi
  | Cons r ->
      if hash r.key land high = high then
        split hash lo live_bs high past_lo
          (Cons { r with rest = past_hi })
          r.rest
      else
        split hash lo live_bs high
          (Cons { r with rest = past_lo })
          past_hi r.rest

let split s lo live_bs high past_bs =
  let past = freeze lo past_bs in
  split s.hash lo live_bs high Nil Nil past

let merge _s lo live_bs high past_bs =
  let past_lo = freeze lo past_bs in
  let past_hi = freeze (lo + high) past_bs in
  let[@tail_mod_cons] rec merge rest = function
    | Nil -> rest
    | Cons r -> Cons { r with rest = merge rest r.rest }
  in
  let ((Nil | Cons _) as past) = merge past_lo past_hi in
  set_if_fresh live_bs lo past

let resize s i =
  let past = s.past in
  if not (Past.is_size past) then
    let p = Past.unsafe_as_table past in
    let live_bs = s.buckets in
    let live_n = Atomic_array.length live_bs in
    let past_bs = p.buckets in
    let past_n = Atomic_array.length past_bs in
    if live_n > past_n then
      split s (i land (past_n - 1)) s.buckets past_n p.buckets
    else if live_n < past_n then
      merge s (i land (live_n - 1)) s.buckets live_n p.buckets
    else copy s i s.buckets p.buckets

(* *)

let rec assoc t key = function
  | Nil -> raise_notrace Not_found
  | Cons r -> if t r.key key then r.value else assoc t key r.rest

let find_exn t key =
  let s = Atomic.get t in
  let h = s.hash key in
  let mask = Atomic_array.length s.buckets - 1 in
  let i = h land mask in
  match Atomic_array.unsafe_fenceless_get s.buckets i with
  | B Nil -> raise_notrace Not_found
  | B (Cons cons_r) ->
      if s.equal cons_r.key key then cons_r.value
      else assoc s.equal key cons_r.rest
  | B (Frozen r) -> assoc s.equal key r.spine
  | B Fresh ->
      resize s i;
      let spine =
        match Atomic_array.unsafe_fenceless_get s.buckets i with
        | B ((Nil | Cons _) as spine) | B (Frozen { spine }) -> spine
        | B Fresh -> failwith "find_exn"
      in
      assoc s.equal key spine

(* *)

let[@inline] non_linearizable_sum counters =
  let accum = ref 0 in
  for i = 0 to Array.length counters - 1 do
    accum := !accum + Atomic.fenceless_get (Array.unsafe_get counters i)
  done;
  !accum

let[@inline] non_linearizable_size s =
  non_linearizable_sum s.non_linearizable_size_delta
  +
  let past = s.past in
  if Past.is_size past then Past.unsafe_as_size past
  else
    let p = Past.unsafe_as_table past in
    non_linearizable_sum p.non_linearizable_size_delta + p.size

let[@inline never] try_resize t s new_capacity size =
  let past =
    let { buckets; non_linearizable_size_delta; _ } = s in
    Past.of_table { buckets; non_linearizable_size_delta; size }
  in
  let buckets = Atomic_array.make new_capacity (B Fresh) in
  let non_linearizable_size_delta =
    Array.init (Array.length s.non_linearizable_size_delta) @@ fun _ ->
    Atomic.make_contended 0
  in
  Atomic.compare_and_set t s
    { s with past; buckets; non_linearizable_size_delta }

let mark_resize_finished s (p : _ Past.table) =
  let rec length s = function Nil -> s | Cons r -> length (s + 1) r.rest in
  let size = ref 0 in
  let i = ref (Atomic_array.length p.buckets - 1) in
  while 0 <= !i do
    begin match Atomic_array.unsafe_fenceless_get p.buckets !i with
    | B (Frozen r) -> size := length !size r.spine
    | _ -> failwith "mark_resize_finished"
    end;
    if (Sys.opaque_identity s).past != Past.of_table p then i := -2 else decr i
  done;
  if !i = -1 then s.past <- Past.of_size !size

let try_finish_resize (p : _ Past.table) s mask =
  let stride = Int64.to_int (Random.bits64 ()) lor 1 land mask in
  let fuel = ref 16 in
  let i = ref stride in
  while !fuel > 0 do
    match Atomic_array.unsafe_fenceless_get s.buckets !i with
    | B (Nil | Cons _) ->
        i := (!i + stride) land mask;
        if !i = stride then fuel := -1
    | B Fresh ->
        decr fuel;
        resize s !i;
        i := (!i + stride) land mask;
        if !i = stride then fuel := -1
    | B (Frozen _) -> fuel := 0
  done;
  if !fuel = -1 then mark_resize_finished s p

let rec adjust_size t s mask delta result =
  let i = Multicore_magic.instantaneous_domain_index () in
  let n = Array.length s.non_linearizable_size_delta in
  if i < n then begin
    let _ : int =
      Atomic.fetch_and_add
        (Array.unsafe_get s.non_linearizable_size_delta i)
        delta
    in
    if Int64.to_int (Random.bits64 ()) land mask = 0 then begin
      let past = s.past in
      if Past.is_size past then begin
        let size = Past.unsafe_as_size past in
        if Atomic.get t == s then begin
          let estimated_size = non_linearizable_size s in
          let capacity = Atomic_array.length s.buckets in
          if capacity < estimated_size && capacity < s.max_buckets then
            try_resize t s (capacity + capacity) size |> ignore
          else if
            s.min_buckets < capacity
            && estimated_size + estimated_size + estimated_size < capacity
          then try_resize t s (capacity lsr 1) size |> ignore
        end
      end
      else begin
        let p = Past.unsafe_as_table past in
        try_finish_resize p s mask
      end
    end;
    result
  end
  else
    let past = s.past in
    if Past.is_size past then
      let new_cs =
        Array.init (n + n + 1) @@ fun i ->
        if i < n then Array.unsafe_get s.non_linearizable_size_delta i
        else Atomic.make_contended 0
      in
      let new_r = { s with non_linearizable_size_delta = new_cs } in
      if Atomic.compare_and_set t s new_r then
        adjust_size t new_r mask delta result
      else
        let _ : int =
          Atomic.fetch_and_add
            (Array.unsafe_get s.non_linearizable_size_delta 0)
            delta
        in
        result
    else
      let p = Past.unsafe_as_table past in
      let _ : int =
        Atomic.fetch_and_add
          (Array.unsafe_get s.non_linearizable_size_delta 0)
          delta
      in
      try_finish_resize p s mask;
      result

(* *)

let rec clear t =
  let s = Atomic.get t in
  let past = s.past in
  if Past.is_size past then begin
    let buckets = Atomic_array.make s.min_buckets (B Nil) in
    let non_linearizable_size_delta =
      Array.init (Array.length s.non_linearizable_size_delta) (fun _ ->
          Atomic.make_contended 0)
    in
    let new_s =
      { s with buckets; non_linearizable_size_delta; past = Past.of_size 0 }
    in
    if not (Atomic.compare_and_set t s new_s) then clear t
  end
  else
    let p = Past.unsafe_as_table past in
    let mask = Atomic_array.length s.buckets - 1 in
    try_finish_resize p s mask;
    clear t

(* *)

let finish_resize s p =
  let mask = Atomic_array.length s.buckets - 1 in
  for i = 0 to mask do
    (* TODO: early exit *)
    resize s i
  done;
  mark_resize_finished s p

let rec to_seq t =
  let s = Atomic.get t in
  let past = s.past in
  if Past.is_size past then begin
    let size = Past.unsafe_as_size past in
    let buckets = Atomic_array.make (Atomic_array.length s.buckets) (B Fresh) in
    let non_linearizable_size_delta =
      Array.init (Array.length s.non_linearizable_size_delta) (fun _ ->
          Atomic.make_contended 0)
    in
    let p =
      {
        Past.buckets = s.buckets;
        non_linearizable_size_delta = s.non_linearizable_size_delta;
        size;
      }
    in
    let new_s =
      { s with buckets; non_linearizable_size_delta; past = Past.of_table p }
    in
    if Atomic.compare_and_set t s new_s then begin
      finish_resize new_s p;
      let snapshot = p.buckets in
      let rec loop i kvs () =
        match kvs with
        | Nil ->
            if i = Atomic_array.length snapshot then Seq.Nil
            else
              loop (i + 1)
                (match Atomic_array.unsafe_fenceless_get snapshot i with
                | B (Frozen spine_r) -> spine_r.spine
                | B (Nil | Cons _ | Fresh) ->
                    (* After resize only [Frozen] values should be left in the
                       old buckets. *)
                    assert false)
                ()
        | Cons r -> Seq.Cons ((r.key, r.value), loop i r.rest)
      in
      loop 0 Nil
    end
    else to_seq t
  end
  else
    let p = Past.unsafe_as_table past in
    finish_resize s p;
    (* TODO: No longer necessary to finish next resize *)
    to_seq t

(* *)

let rec exists t key = function
  | Nil -> false
  | Cons r ->
      let result = t r.key key in
      if result then result else exists t key r.rest

let rec try_add t key value backoff =
  let s = Atomic.get t in
  let h = s.hash key in
  let mask = Atomic_array.length s.buckets - 1 in
  let i = h land mask in
  match Atomic_array.unsafe_fenceless_get s.buckets i with
  | B Nil ->
      let after = Cons { key; value; rest = Nil } in
      if Atomic_array.unsafe_compare_and_set s.buckets i (B Nil) (B after) then
        adjust_size t s mask 1 true
      else try_add t key value (Backoff.once backoff)
  | B (Cons _ as before) ->
      if exists s.equal key before then false
      else
        let after = Cons { key; value; rest = before } in
        if Atomic_array.unsafe_compare_and_set s.buckets i (B before) (B after)
        then adjust_size t s mask 1 true
        else try_add t key value (Backoff.once backoff)
  | B Fresh ->
      resize s i;
      try_add t key value backoff
  | B (Frozen _) -> try_add t key value backoff

(* *)

let rec set_exn t key future backoff =
  let s = Atomic.get t in
  let h = s.hash key in
  let mask = Atomic_array.length s.buckets - 1 in
  let i = h land mask in
  match Atomic_array.unsafe_fenceless_get s.buckets i with
  | B Nil -> raise_notrace Not_found
  | B (Cons cons_r as before) -> begin
      if s.equal cons_r.key key then
        let after = Cons { key; value = future; rest = cons_r.rest } in
        if Atomic_array.unsafe_compare_and_set s.buckets i (B before) (B after)
        then cons_r.value
        else set_exn t key future (Backoff.once backoff)
      else
        let[@tail_mod_cons] rec reassoc =
         fun equal key future -> function
           | Nil -> raise_notrace Not_found
           | Cons r ->
               if equal key r.key then Cons { r with value = future }
               else Cons { r with rest = reassoc equal key future r.rest }
        in
        match reassoc s.equal key future cons_r.rest with
        | rest ->
            let after = Cons { cons_r with rest } in
            if
              Atomic_array.unsafe_compare_and_set s.buckets i (B before)
                (B after)
            then assoc s.equal key cons_r.rest
            else set_exn t key future (Backoff.once backoff)
        | exception Not_found -> raise_notrace Not_found
    end
  | B Fresh ->
      resize s i;
      set_exn t key future backoff
  | B (Frozen _) -> set_exn t key future backoff

(* *)

let rec remove_exn t key backoff =
  let s = Atomic.get t in
  let h = s.hash key in
  let mask = Atomic_array.length s.buckets - 1 in
  let i = h land mask in
  match Atomic_array.unsafe_fenceless_get s.buckets i with
  | B Nil -> raise_notrace Not_found
  | B (Cons cons_r as before) -> begin
      if s.equal cons_r.key key then
        if
          Atomic_array.unsafe_compare_and_set s.buckets i (B before)
            (B cons_r.rest)
        then
          let res = cons_r.value in
          adjust_size t s mask (-1) res
        else remove_exn t key (Backoff.once backoff)
      else
        let[@tail_mod_cons] rec dissoc =
         fun equal key -> function
           | Nil -> raise_notrace Not_found
           | Cons r ->
               if equal key r.key then r.rest
               else Cons { r with rest = dissoc equal key r.rest }
        in
        match dissoc s.equal key cons_r.rest with
        | (Nil | Cons _) as rest ->
            if
              Atomic_array.unsafe_compare_and_set s.buckets i (B before)
                (B (Cons { cons_r with rest }))
            then
              let res = assoc s.equal key cons_r.rest in
              adjust_size t s mask (-1) res
            else remove_exn t key (Backoff.once backoff)
        | exception Not_found -> raise_notrace Not_found
    end
  | B Fresh ->
      resize s i;
      remove_exn t key backoff
  | B (Frozen _) -> remove_exn t key backoff

(* *)

let non_linearizable_length t = non_linearizable_size (Atomic.get t)

(* *)

let[@inline] try_add t key value = try_add t key value Backoff.default

(* *)

let[@inline] set_exn t key value = set_exn t key value Backoff.default

(* *)

let[@inline] remove_exn t key = remove_exn t key Backoff.default
