open QCheck
open STM
module Htbl = Picos_htbl

let () =
  (* Basics *)
  Random.self_init ();
  let t = Picos_htbl.create () in
  assert (Picos_htbl.try_add t "Basics" 101);
  assert (Picos_htbl.try_add t "Answer" 42);
  assert (101 = Picos_htbl.remove_exn t "Basics");
  assert (not (Picos_htbl.try_remove t "Basics"));
  assert (Picos_htbl.remove_all t |> List.of_seq = [ ("Answer", 42) ]);
  assert (Picos_htbl.to_seq t |> List.of_seq = []);
  [ "One"; "Two"; "Three" ]
  |> List.iteri (fun v k -> assert (Picos_htbl.try_add t k v));
  assert (
    Picos_htbl.to_seq t |> List.of_seq
    |> List.sort (fun l r -> String.compare (fst l) (fst r))
    = [ ("One", 0); ("Three", 2); ("Two", 1) ])

module Int = struct
  include Int

  let hash = Fun.id
end

module Spec = struct
  type cmd =
    | Try_add of int
    | Mem of int
    | Try_remove of int
    | To_keys
    | Remove_all

  let show_cmd c =
    match c with
    | Try_add x -> "Try_add " ^ string_of_int x
    | Mem x -> "Mem " ^ string_of_int x
    | Try_remove x -> "Try_remove " ^ string_of_int x
    | To_keys -> "To_keys"
    | Remove_all -> "Remove_all"

  module State = Set.Make (Int)

  type state = State.t
  type sut = (int, unit) Htbl.t

  let arb_cmd _s =
    [
      Gen.int_bound 10 |> Gen.map (fun x -> Try_add x);
      Gen.int_bound 10 |> Gen.map (fun x -> Mem x);
      Gen.int_bound 10 |> Gen.map (fun x -> Try_remove x);
      Gen.return To_keys;
      Gen.return Remove_all;
    ]
    |> Gen.oneof |> make ~print:show_cmd

  let init_state = State.empty
  let init_sut () = Htbl.create ~hashed_type:(module Int) ()
  let cleanup _ = ()

  let next_state c s =
    match c with
    | Try_add x -> State.add x s
    | Mem _ -> s
    | Try_remove x -> State.remove x s
    | To_keys -> s
    | Remove_all -> State.empty

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
    | To_keys -> Res (seq int, Htbl.to_seq d |> Seq.map fst)
    | Remove_all -> Res (seq int, Htbl.remove_all d |> Seq.map fst)

  let postcond c (s : state) res =
    match (c, res) with
    | Try_add x, Res ((Bool, _), res) -> res <> State.mem x s
    | Mem x, Res ((Bool, _), res) -> res = State.mem x s
    | Try_remove x, Res ((Bool, _), res) -> res = State.mem x s
    | To_keys, Res ((Seq Int, _), res) -> State.equal (State.of_seq res) s
    | Remove_all, Res ((Seq Int, _), res) -> State.equal (State.of_seq res) s
    | _, _ -> false
end

let () = Stm_run.run ~name:"Picos_htbl" ~verbose:true (module Spec) |> exit
