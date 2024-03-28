type ('k, 'v, _) tdt =
  | Nil : ('k, 'v, [> `Nil ]) tdt
  | Cons : {
      key : 'k;
      value : 'v;
      rest : ('k, 'v, [ `Cons | `Nil ]) tdt;
    }
      -> ('k, 'v, [> `Cons ]) tdt
  | Fresh : ('k, 'v, [> `Fresh ]) tdt
  | Split : ('k, 'v, [ `Cons | `Nil ]) tdt -> ('k, 'v, [> `Split ]) tdt

type ('k, 'v) bucket =
  | B : ('k, 'v, [< `Nil | `Cons | `Fresh | `Split ]) tdt -> ('k, 'v) bucket
[@@unboxed]

type ('k, 'v) t = {
  hash : 'k -> int;
  buckets : ('k, 'v) bucket Atomic.t array Atomic.t;
  equal : 'k -> 'k -> bool;
  non_linearizable_size : int Atomic.t array Atomic.t;
}

let create ~equal ~hash () =
  let buckets =
    Atomic.make [| Atomic.make (B Nil) |> Multicore_magic.copy_as_padded |]
    |> Multicore_magic.copy_as_padded
  in
  let non_linearizable_size =
    Atomic.make [| Atomic.make 0 |> Multicore_magic.copy_as_padded |]
    |> Multicore_magic.copy_as_padded
  in
  { hash; buckets; equal; non_linearizable_size }
  |> Multicore_magic.copy_as_padded

(* *)

let rec filter_exn changed hash msk chk = function
  | Nil -> if changed then Nil else raise_notrace Not_found
  | Cons r ->
      let h = hash r.key in
      if h land msk = chk then
        Cons { r with rest = filter_exn changed hash msk chk r.rest }
      else filter_exn true hash msk chk r.rest

let filter hash msk chk spine =
  try filter_exn false hash msk chk spine with Not_found -> spine

let rec try_finish t bs i0 high =
  if i0 < high then begin
    let i1 = i0 + high in
    let b1 = Array.unsafe_get bs i1 in
    match Atomic.get b1 with
    | B Fresh -> begin
        let b0 = Array.unsafe_get bs i0 in
        match Atomic.get b0 with
        | B (Split spine as before) ->
            if Atomic.get t.buckets != bs then finish t
            else begin
              let after = filter t.hash (high + high - 1) i1 spine in
              if not (Atomic.compare_and_set b1 (B Fresh) (B after)) then
                try_finish t bs i0 high
              else
                let after = filter t.hash (high + high - 1) i0 spine in
                if Atomic.compare_and_set b0 (B before) (B after) then
                  try_finish t bs (i0 + 1) high
                else try_finish t bs i0 high
            end
        | B ((Nil | Cons _) as before) ->
            if Atomic.get t.buckets != bs then finish t
            else begin
              Atomic.compare_and_set b0 (B before) (B (Split before)) |> ignore;
              try_finish t bs i0 high
            end
        | B Fresh -> assert false
      end
    | B (Split _) -> finish t
    | B (Nil | Cons _) -> begin
        let b0 = Array.unsafe_get bs i0 in
        match Atomic.get b0 with
        | B (Split spine as before) ->
            if Atomic.get t.buckets != bs then finish t
            else
              let after = filter t.hash (high + high - 1) i0 spine in
              if Atomic.compare_and_set b0 (B before) (B after) then
                try_finish t bs (i0 + 1) high
              else try_finish t bs i0 high
        | B (Nil | Cons _) -> try_finish t bs (i0 + 1) high
        | B Fresh -> assert false
      end
  end

and finish t =
  let bs = Atomic.get t.buckets in
  let high = Array.length bs lsr 1 in
  try_finish t bs 0 high

let grow t old_bs =
  finish t;
  if old_bs == Atomic.get t.buckets then begin
    let n = Array.length old_bs in
    let new_bs =
      Array.init (n + n) @@ fun i ->
      if i < n then Array.unsafe_get old_bs i
      else Atomic.make (B Fresh) |> Multicore_magic.copy_as_padded
    in
    Atomic.compare_and_set t.buckets old_bs new_bs |> ignore
  end

(* *)

let rec assoc t key = function
  | Nil -> raise_notrace Not_found
  | Cons r -> if t key r.key then r.value else assoc t key r.rest

let rec find_exn t key h =
  let bs = Atomic.get t.buckets in
  let mask = Array.length bs - 1 in
  let i = h land mask in
  let b = Array.unsafe_get bs i in
  match Atomic.get b with
  | B ((Nil | Cons _) as spine) -> assoc t.equal key spine
  | B (Fresh | Split _) ->
      finish t;
      find_exn t key h

let find_exn t key = find_exn t key (t.hash key)

(* *)

let rec counter t =
  let i = Multicore_magic.instantaneous_domain_index () in
  let cs = Atomic.get t in
  let n = Array.length cs in
  if i < n then Array.unsafe_get cs i
  else
    let new_cs =
      Array.init (n + n) @@ fun i ->
      if i < n then Array.unsafe_get cs i
      else Atomic.make 0 |> Multicore_magic.copy_as_padded
    in
    Atomic.compare_and_set t cs new_cs |> ignore;
    counter t

let rec estimated_size cs n sum =
  let n = n - 1 in
  if 0 <= n then estimated_size cs n (sum + Atomic.get (Array.unsafe_get cs n))
  else sum

let estimated_size t =
  let cs = Atomic.get t in
  let n = Array.length cs - 1 in
  estimated_size cs n (Atomic.get (Array.unsafe_get cs n))

(* *)

let rec mem t key = function
  | Nil -> false
  | Cons r -> t key r.key || mem t key r.rest

let rec try_add t key value h backoff =
  let bs = Atomic.get t.buckets in
  let mask = Array.length bs - 1 in
  let i = h land mask in
  let b = Array.unsafe_get bs i in
  match Atomic.get b with
  | B ((Nil | Cons _) as before) ->
      (not (mem t.equal key before))
      &&
      let after = Cons { key; value; rest = before } in
      if Atomic.compare_and_set b (B before) (B after) then begin
        Atomic.incr (counter t.non_linearizable_size);
        if
          Random.bits () land mask = 0
          && estimated_size t.non_linearizable_size > mask
        then grow t bs;
        true
      end
      else try_add t key value h (Backoff.once backoff)
  | B (Fresh | Split _) ->
      finish t;
      try_add t key value h Backoff.default

let try_add t key value =
  let h = t.hash key in
  try_add t key value h Backoff.default

(* *)

let[@tail_mod_cons] rec dissoc t key = function
  | Nil -> raise_notrace Not_found
  | Cons r ->
      if t key r.key then r.rest else Cons { r with rest = dissoc t key r.rest }

let rec try_remove t key h backoff =
  let bs = Atomic.get t.buckets in
  let mask = Array.length bs - 1 in
  let i = h land mask in
  let b = Array.unsafe_get bs i in
  match Atomic.get b with
  | B ((Nil | Cons _) as before) -> begin
      match dissoc t.equal key before with
      | after ->
          if Atomic.compare_and_set b (B before) (B after) then begin
            Atomic.decr (counter t.non_linearizable_size);
            true
          end
          else try_remove t key h (Backoff.once backoff)
      | exception Not_found -> false
    end
  | B (Fresh | Split _) ->
      finish t;
      try_remove t key h Backoff.default

let try_remove t key =
  let h = t.hash key in
  try_remove t key h Backoff.default
