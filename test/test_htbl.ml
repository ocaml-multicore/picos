open QCheck
open STM
module Htbl = Picos_htbl

module Spec = struct
  type cmd = Try_add of int | Mem of int | Try_remove of int

  let show_cmd c =
    match c with
    | Try_add x -> "Try_add " ^ string_of_int x
    | Mem x -> "Mem " ^ string_of_int x
    | Try_remove x -> "Try_remove " ^ string_of_int x

  type state = int list
  type sut = (int, unit) Htbl.t

  let arb_cmd _s =
    [
      Gen.int_bound 10 |> Gen.map (fun x -> Try_add x);
      Gen.int_bound 10 |> Gen.map (fun x -> Mem x);
      Gen.int_bound 10 |> Gen.map (fun x -> Try_remove x);
    ]
    |> Gen.oneof |> make ~print:show_cmd

  let init_state = []
  let init_sut () = Htbl.create ~equal:Int.equal ~hash:Fun.id ()
  let cleanup _ = ()

  let next_state c s =
    match c with
    | Try_add x -> if List.exists (( = ) x) s then s else x :: s
    | Mem _ -> s
    | Try_remove x ->
        let[@tail_mod_cons] rec drop_first = function
          | [] -> []
          | x' :: xs -> if x = x' then xs else x' :: drop_first xs
        in
        drop_first s

  let precond _ _ = true

  let run c d =
    match c with
    | Try_add x -> Res (bool, Htbl.try_add d x ())
    | Mem x ->
        Res
          ( bool,
            match Htbl.find_exn d x with
            | _ -> true
            | exception Not_found -> false )
    | Try_remove x -> Res (bool, Htbl.try_remove d x)

  let postcond c (s : state) res =
    match (c, res) with
    | Try_add x, Res ((Bool, _), res) -> res = List.for_all (( != ) x) s
    | Mem x, Res ((Bool, _), res) -> res = List.exists (( = ) x) s
    | Try_remove x, Res ((Bool, _), res) -> res = List.exists (( = ) x) s
    | _, _ -> false
end

let () =
  let count = if Sys.int_size <= 32 then 100 else 1000 in
  Stm_run.run ~count ~name:"Picos_htbl" ~verbose:true (module Spec) |> exit
