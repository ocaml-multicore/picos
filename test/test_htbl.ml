open QCheck
open STM
module Htbl = Picos_aux_htbl

let () =
  (* Basics *)
  Random.self_init ();
  let t = Htbl.create () in
  assert (Htbl.try_add t "Basics" 101);
  assert (Htbl.try_add t "Answer" 42);
  assert (101 = Htbl.remove_exn t "Basics");
  assert (not (Htbl.try_remove t "Basics"));
  assert (Htbl.remove_all t |> List.of_seq = [ ("Answer", 42) ]);
  assert (Htbl.to_seq t |> List.of_seq = []);
  [ "One"; "Two"; "Three" ]
  |> List.iteri (fun v k -> assert (Htbl.try_add t k v));
  assert (
    Htbl.to_seq t |> List.of_seq
    |> List.sort (fun l r -> String.compare (fst l) (fst r))
    = [ ("One", 0); ("Three", 2); ("Two", 1) ])

module Int = struct
  include Int

  let hash = Fun.id
end

module Spec = struct
  include SpecDefaults

  type cmd =
    | Try_add of int
    | Mem of int
    | Remove_opt of int
    | Set_opt of int
    | To_keys
    | Remove_all

  let show_cmd c =
    match c with
    | Try_add x -> "Try_add " ^ string_of_int x
    | Mem x -> "Mem " ^ string_of_int x
    | Remove_opt x -> "Remove_opt " ^ string_of_int x
    | Set_opt x -> "Set_opt " ^ string_of_int x
    | To_keys -> "To_keys"
    | Remove_all -> "Remove_all"

  module State = Set.Make (Int)

  type state = State.t
  type sut = (int, int) Htbl.t

  let arb_cmd _s =
    [
      Gen.int_bound 10 |> Gen.map (fun x -> Try_add x);
      Gen.int_bound 10 |> Gen.map (fun x -> Mem x);
      Gen.int_bound 10 |> Gen.map (fun x -> Remove_opt x);
      Gen.int_bound 10 |> Gen.map (fun x -> Set_opt x);
      Gen.return To_keys;
      Gen.return Remove_all;
    ]
    |> Gen.oneof |> make ~print:show_cmd

  let init_state = State.empty
  let init_sut () = Htbl.create ~hashed_type:(module Int) ()

  let next_state c s =
    match c with
    | Try_add x -> State.add x s
    | Mem _ -> s
    | Remove_opt x -> State.remove x s
    | Set_opt _ -> s
    | To_keys -> s
    | Remove_all -> State.empty

  let run c d =
    match c with
    | Try_add x -> Res (bool, Htbl.try_add d x x)
    | Mem x ->
        Res
          ( bool,
            match Htbl.find_exn d x with
            | _ -> true
            | exception Not_found -> false )
    | Remove_opt x ->
        Res
          ( option int,
            match Htbl.remove_exn d x with
            | x -> Some x
            | exception Not_found -> None )
    | Set_opt x ->
        Res
          ( option int,
            match Htbl.set_exn d x x with
            | x -> Some x
            | exception Not_found -> None )
    | To_keys -> Res (seq int, Htbl.to_seq d |> Seq.map fst)
    | Remove_all -> Res (seq int, Htbl.remove_all d |> Seq.map fst)

  let postcond c (s : state) res =
    match (c, res) with
    | Try_add x, Res ((Bool, _), res) -> res <> State.mem x s
    | Mem x, Res ((Bool, _), res) -> res = State.mem x s
    | Remove_opt x, Res ((Option Int, _), res) -> begin
        match res with
        | None -> not (State.mem x s)
        | Some x' -> x == x' && State.mem x s
      end
    | Set_opt x, Res ((Option Int, _), res) -> begin
        match res with
        | None -> not (State.mem x s)
        | Some x' -> x == x' && State.mem x s
      end
    | To_keys, Res ((Seq Int, _), res) -> State.equal (State.of_seq res) s
    | Remove_all, Res ((Seq Int, _), res) -> State.equal (State.of_seq res) s
    | _, _ -> false
end

let () = Stm_run.run ~name:"Htbl" (module Spec) |> exit
