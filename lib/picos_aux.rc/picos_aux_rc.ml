include Intf

let[@inline never] created () =
  invalid_arg "resource already previously created"

let[@inline never] disposed () =
  invalid_arg "resource already previously disposed"

let bt =
  if Printexc.backtrace_status () then None else Some (Printexc.get_callstack 0)

let count_shift = 2
let count_1 = 1 lsl count_shift
let dispose_bit = 0b01
let closed_bit = 0b10

module Htbl = Picos_aux_htbl

module Make (Resource : Resource) () : S with module Resource = Resource =
struct
  module Resource = Resource

  type entry = { count_and_bits : int; bt : Printexc.raw_backtrace }

  let ht = Htbl.create ~hashed_type:(module Resource) ()

  type t = Resource.t

  let create ?(dispose = true) t =
    let bt =
      match bt with Some bt -> bt | None -> Printexc.get_callstack 15
    in
    if
      Htbl.try_add ht t
        (Atomic.make { count_and_bits = count_1 lor Bool.to_int dispose; bt })
    then t
    else begin
      (* We assume resources may only be reused after they have been
         disposed. *)
      created ()
    end

  let unsafe_get = Fun.id

  let rec incr t entry backoff =
    let before = Atomic.get entry in
    if
      before.count_and_bits < count_1
      || before.count_and_bits land closed_bit <> 0
    then disposed ()
    else
      let count_and_bits = before.count_and_bits + count_1 in
      let after = { before with count_and_bits } in
      if not (Atomic.compare_and_set entry before after) then
        incr t entry (Backoff.once backoff)

  let incr t =
    match Htbl.find_exn ht t with
    | exception Not_found -> disposed ()
    | entry -> incr t entry Backoff.default

  let rec decr closed_bit t entry backoff =
    let before = Atomic.get entry in
    let count_and_bits = (before.count_and_bits - count_1) lor closed_bit in
    if count_and_bits < 0 then disposed ()
    else
      let after = { before with count_and_bits } in
      if not (Atomic.compare_and_set entry before after) then
        decr closed_bit t entry (Backoff.once backoff)
      else if count_and_bits < count_1 then begin
        Htbl.try_remove ht t |> ignore;
        (* We must dispose the resource as the last step, because the value
           might be reused after it has been disposed. *)
        if after.count_and_bits land dispose_bit <> 0 then Resource.dispose t
      end

  let decr ?close t =
    match Htbl.find_exn ht t with
    | exception Not_found -> disposed ()
    | entry ->
        decr
          (match close with None | Some false -> 0 | Some true -> closed_bit)
          t entry Backoff.default

  type info = {
    resource : Resource.t;
    count : int;
    closed : bool;
    dispose : bool;
    bt : Printexc.raw_backtrace;
  }

  let infos () =
    Htbl.to_seq ht
    |> Seq.map @@ fun (resource, entry) ->
       let { count_and_bits; bt } = Atomic.get entry in
       let count = count_and_bits lsr count_shift in
       let closed = count_and_bits land closed_bit <> 0 in
       let dispose = count_and_bits land dispose_bit <> 0 in
       { resource; count; closed; dispose; bt }
end
