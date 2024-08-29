open QCheck
open STM
module Queue = Picos_aux_mpmcq

let () =
  let q = Queue.create () in
  Queue.push q 101;
  Queue.push q 42;
  assert (Queue.pop_exn q = 101);
  Queue.push q 76;
  assert (Queue.pop_exn q = 42);
  assert (Queue.pop_exn q = 76);
  match Queue.pop_exn q with _ -> assert false | exception Queue.Empty -> ()

module Spec = struct
  type cmd = Push of int | Push_head of int | Pop_opt | Length

  let show_cmd = function
    | Push x -> "Push " ^ string_of_int x
    | Push_head x -> "Push_head " ^ string_of_int x
    | Pop_opt -> "Pop_opt"
    | Length -> "Length"

  module State = struct
    type t = int list * int list

    let push x (h, t) = if h == [] then ([ x ], []) else (h, x :: t)
    let push_head x (h, t) = (x :: h, t)
    let peek_opt (h, _) = match h with x :: _ -> Some x | [] -> None

    let drop ((h, t) as s) =
      match h with [] -> s | [ _ ] -> (List.rev t, []) | _ :: h -> (h, t)

    let length (h, t) = List.length h + List.length t
  end

  type state = State.t
  type sut = int Queue.t

  let arb_cmd _s =
    [
      Gen.int_range 1 1000 |> Gen.map (fun x -> Push x);
      Gen.int_range 1 1000 |> Gen.map (fun x -> Push_head x);
      Gen.return Pop_opt;
      Gen.return Length;
    ]
    |> Gen.oneof |> make ~print:show_cmd

  let init_state = ([], [])
  let init_sut () = Queue.create ~padded:true ()
  let cleanup _ = ()

  let next_state c s =
    match c with
    | Push x -> State.push x s
    | Push_head x -> State.push_head x s
    | Pop_opt -> State.drop s
    | Length -> s

  let precond _ _ = true

  let run c d =
    match c with
    | Push x -> Res (unit, Queue.push d x)
    | Push_head x -> Res (unit, Queue.push_head d x)
    | Pop_opt ->
        Res
          ( option int,
            match Queue.pop_exn d with
            | v -> Some v
            | exception Queue.Empty -> None )
    | Length -> Res (int, Queue.length d)

  let postcond c (s : state) res =
    match (c, res) with
    | Push _x, Res ((Unit, _), ()) -> true
    | Push_head _x, Res ((Unit, _), ()) -> true
    | Pop_opt, Res ((Option Int, _), res) -> res = State.peek_opt s
    | Length, Res ((Int, _), res) -> res = State.length s
    | _, _ -> false
end

let () = Stm_run.run ~name:"Mpmcq" (module Spec) |> exit
