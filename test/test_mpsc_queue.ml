open Foundation

module Spec = struct
  type cmd = Enq of int | Deq

  let show_cmd c =
    match c with Enq i -> "Enq " ^ string_of_int i | Deq -> "Deq"

  type state = int list
  type sut = int Mpsc_queue.t

  let arb_cmd _s =
    QCheck.(
      make ~print:show_cmd
        (Gen.oneof [ Gen.map (fun i -> Enq i) Gen.nat; Gen.return Deq ]))

  let init_state = []
  let init_sut () = Mpsc_queue.create ()
  let cleanup _ = ()

  let next_state c s =
    match c with
    | Enq i -> s @ [ i ]
    | Deq -> ( match s with _ :: s -> s | [] -> [])

  let precond _ _ = true

  let run c d =
    let open STM in
    match c with
    | Enq i -> Res (unit, Mpsc_queue.enqueue d i)
    | Deq ->
        Res
          ( option int,
            match Mpsc_queue.dequeue d with
            | i -> Some i
            | exception Mpsc_queue.Empty -> None )

  let postcond c (s : state) res =
    let open STM in
    match (c, res) with
    | Enq _, Res ((Unit, _), ()) -> true
    | Deq, Res ((Option Int, _), res) ->
        (match s with [] -> None | x :: _ -> Some x) = res
    | _, _ -> false
end

module Seq = STM_sequential.Make (Spec)

let () =
  let count = 1000 in
  QCheck_base_runner.run_tests_main
    [ Seq.agree_test ~count ~name:"STM Mpsc_queue test sequential" ]
