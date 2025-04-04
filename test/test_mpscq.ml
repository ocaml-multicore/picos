open QCheck
open STM
module Queue = Picos_aux_mpscq

module Spec = struct
  include SpecDefaults

  type cmd = Push of int | Push_head of int | Pop | Pop_all

  let show_cmd c =
    match c with
    | Push i -> "Push " ^ string_of_int i
    | Push_head i -> "Push_head " ^ string_of_int i
    | Pop -> "Pop"
    | Pop_all -> "Pop_all"

  type state = int list
  type sut = int Queue.t

  let producer_cmd _s =
    [
      Gen.nat |> Gen.map (fun i -> Push i);
      Gen.nat |> Gen.map (fun i -> Push_head i);
    ]
    |> Gen.oneof |> make ~print:show_cmd

  let arb_cmd _s =
    [
      Gen.nat |> Gen.map (fun i -> Push i);
      Gen.nat |> Gen.map (fun i -> Push_head i);
      Gen.return Pop;
      Gen.return Pop_all;
    ]
    |> Gen.oneof |> make ~print:show_cmd

  let init_state = []
  let init_sut () = Queue.create ()

  let next_state c s =
    match c with
    | Push i -> s @ [ i ]
    | Push_head i -> i :: s
    | Pop -> ( match s with _ :: s -> s | [] -> [])
    | Pop_all -> []

  let run c d =
    match c with
    | Push i -> Res (unit, Queue.push d i)
    | Push_head i -> Res (unit, Queue.push_head d i)
    | Pop ->
        Res
          ( option int,
            match Queue.pop_exn d with
            | i -> Some i
            | exception Queue.Empty -> None )
    | Pop_all -> Res (list int, Queue.pop_all d |> List.of_seq)

  let postcond c (s : state) res =
    match (c, res) with
    | Push _, Res ((Unit, _), ()) -> true
    | Push_head _, Res ((Unit, _), ()) -> true
    | Pop, Res ((Option Int, _), res) ->
        (match s with [] -> None | x :: _ -> Some x) = res
    | Pop_all, Res ((List Int, _), res) -> res = s
    | _, _ -> false
end

let () =
  let make_domain ~count ~name
      (module Dom : Stm_run.STM_domain
        with type Spec.cmd = Spec.cmd
         and type Spec.state = Spec.state
         and type Spec.sut = Spec.sut) =
    let arb_cmds_par =
      Dom.arb_triple 20 12 Spec.arb_cmd Spec.arb_cmd Spec.producer_cmd
    in
    let agree_test_par_asym ~count ~name =
      let rep_count = 50 in
      QCheck.Test.make ~retries:10 ~count ~name arb_cmds_par @@ fun triple ->
      QCheck.assume (Dom.all_interleavings_ok triple);
      Util.repeat rep_count Dom.agree_prop_par_asym triple
    in
    [ agree_test_par_asym ~count ~name:(name ^ " parallel") ]
  in
  Stm_run.run ~name:"Mpscq" ~make_domain (module Spec) |> exit
