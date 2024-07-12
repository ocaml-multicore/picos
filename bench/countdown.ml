module Atomic = Multicore_magic.Transparent_atomic

type t = int Atomic.t array

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

let create ~n_domains () =
  if n_domains < 1 then invalid_arg "n_domains < 1";
  let n = ceil_pow_2_minus_1 n_domains in
  let atomics = Array.init n_domains (fun _ -> Atomic.make_contended 0) in
  Array.init n @@ fun i -> Array.unsafe_get atomics (i mod n_domains)

let rec arity t i =
  if i < Array.length t && Array.unsafe_get t i != Array.unsafe_get t 0 then
    arity t (i + 1)
  else i

let[@inline] arity t = arity t 1

let non_atomic_set t count =
  if count < 0 then invalid_arg "count < 0";
  let n = arity t in
  let d = count / n in
  let j = count - (n * d) in
  for i = 0 to n - 1 do
    Atomic.set (Array.unsafe_get t i) (d + Bool.to_int (i < j))
  done

let rec get t count i =
  if i < Array.length t && Array.unsafe_get t i != Array.unsafe_get t 0 then
    get t (count + Int.max 0 (Atomic.get (Array.unsafe_get t i))) (i + 1)
  else count

let[@inline] get t = get t (Int.max 0 (Atomic.get (Array.unsafe_get t 0))) 1

let rec alloc t ~batch i =
  if i < Array.length t then
    let c = Array.unsafe_get t i in
    if 0 < Atomic.get c then
      let n = Atomic.fetch_and_add c (-batch) in
      if 0 < n then Int.min n batch else alloc t ~batch (i + 1)
    else alloc t ~batch (i + 1)
  else 0

let[@inline] alloc t ~domain_index ~batch =
  let c = Array.unsafe_get t domain_index in
  if 0 < Atomic.get c then
    let n = Atomic.fetch_and_add c (-batch) in
    if 0 < n then Int.min n batch else alloc t ~batch 0
  else alloc t ~batch 0
