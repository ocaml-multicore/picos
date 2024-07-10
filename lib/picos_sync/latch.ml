open Picos

let[@inline never] negative_count () = invalid_arg "negative count"
let[@inline never] zero_count () = invalid_arg "zero count"

type t = { count : int Atomic.t; computation : unit Computation.t }

let zero = Atomic.make 0

let create ?padded n =
  if n < 0 then negative_count ();
  let count =
    if 0 < n then Atomic.make n |> Multicore_magic.copy_as ?padded else zero
  and computation =
    if n <= 0 then Computation.finished else Computation.create ()
  in
  Multicore_magic.copy_as ?padded { count; computation }

type _ result =
  | Unit : unit result  (** [Unit] is physically same as [()]. *)
  | Bool : bool result  (** [Bool] is physically same as [true]. *)

let rec try_decr : type a. _ -> a result -> _ -> a =
 fun t result backoff ->
  let n = Atomic.get t.count in
  if 0 < n then
    if Atomic.compare_and_set t.count n (n - 1) then begin
      if n = 1 then Computation.finish t.computation;
      match result with Unit -> () | Bool -> true
    end
    else try_decr t result (Backoff.once backoff)
  else match result with Unit -> zero_count () | Bool -> false

let rec try_incr : type a. _ -> a result -> _ -> a =
 fun t result backoff ->
  let n = Atomic.get t.count in
  if 0 < n then
    if Atomic.compare_and_set t.count n (n + 1) then
      match result with Unit -> () | Bool -> true
    else try_incr t result (Backoff.once backoff)
  else match result with Unit -> zero_count () | Bool -> false

let decr t = try_decr t Unit Backoff.default
let incr t = try_incr t Unit Backoff.default
let try_decr t = try_decr t Bool Backoff.default
let try_incr t = try_incr t Bool Backoff.default
let await t = Computation.await t.computation
let await_evt t = Event.from_computation t.computation
