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
      Htbl.try_add ht t { count_and_bits = count_1 lor Bool.to_int dispose; bt }
    then t
    else created ()

  let rec incr t backoff =
    match Htbl.find_exn ht t with
    | before ->
        if before.count_and_bits land closed_bit <> 0 then disposed ()
        else
          let count_and_bits = before.count_and_bits + count_1 in
          let after = { before with count_and_bits } in
          if not (Htbl.try_compare_and_set ht t before after) then
            incr t (Backoff.once backoff)
    | exception Not_found -> disposed ()

  let rec decr closed_bit t backoff =
    match Htbl.find_exn ht t with
    | before ->
        if before.count_and_bits < count_1 * 2 then
          if Htbl.try_compare_and_remove ht t before then begin
            if before.count_and_bits land dispose_bit <> 0 then
              Resource.dispose t
          end
          else decr closed_bit t (Backoff.once backoff)
        else
          let count_and_bits =
            (before.count_and_bits - count_1) lor closed_bit
          in
          let after = { before with count_and_bits } in
          if not (Htbl.try_compare_and_set ht t before after) then
            decr closed_bit t (Backoff.once backoff)
    | exception Not_found -> disposed ()

  let[@inline] incr t = incr t Backoff.default

  let[@inline] decr ?close t =
    decr
      (match close with None | Some false -> 0 | Some true -> closed_bit)
      t Backoff.default

  let unsafe_get = Fun.id

  type info = {
    resource : Resource.t;
    count : int;
    closed : bool;
    dispose : bool;
    bt : Printexc.raw_backtrace;
  }

  let infos () =
    Htbl.to_seq ht
    |> Seq.map @@ fun (resource, { count_and_bits; bt }) ->
       let count = count_and_bits lsr count_shift in
       let closed = count_and_bits land closed_bit <> 0 in
       let dispose = count_and_bits land dispose_bit <> 0 in
       { resource; count; closed; dispose; bt }
end
