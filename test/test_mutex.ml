open QCheck
open STM
module Mutex = Picos_std_sync.Mutex

module Spec = struct
  include SpecDefaults
  include Stm_wrap

  type cmd = Push of int | Pop_opt | Top_opt | Length

  let show_cmd = function
    | Push x -> "Push " ^ string_of_int x
    | Pop_opt -> "Pop_opt"
    | Top_opt -> "Top_opt"
    | Length -> "Length"

  module State = struct
    type t = int list

    let push x xs = x :: xs
    let pop_opt = function _ :: xs -> xs | [] -> []
    let top_opt = function x :: _ -> Some x | [] -> None
    let length = List.length
  end

  type state = State.t
  type sut = { stack : int Stack.t; mutex : Mutex.t }

  let arb_cmd _s =
    [
      Gen.int_range 1 1000 |> Gen.map (fun x -> Push x);
      Gen.return Pop_opt;
      Gen.return Top_opt;
      Gen.return Length;
    ]
    |> Gen.oneof |> make ~print:show_cmd

  let init_state = []
  let init_sut () = { stack = Stack.create (); mutex = Mutex.create () }

  let next_state c s =
    match c with
    | Push x -> State.push x s
    | Pop_opt -> State.pop_opt s
    | Top_opt -> s
    | Length -> s

  let run c d =
    match c with
    | Push x ->
        Mutex.lock d.mutex;
        Stack.push x d.stack;
        Mutex.unlock d.mutex;
        Res (unit, ())
    | Pop_opt ->
        Mutex.lock d.mutex;
        let result = Stack.pop_opt d.stack in
        Mutex.unlock d.mutex;
        Res (option int, result)
    | Top_opt ->
        Mutex.lock d.mutex;
        let result = Stack.top_opt d.stack in
        Mutex.unlock d.mutex;
        Res (option int, result)
    | Length ->
        Mutex.lock d.mutex;
        let result = Stack.length d.stack in
        Mutex.unlock d.mutex;
        Res (int, result)

  let postcond c (s : state) res =
    match (c, res) with
    | Push _x, Res ((Unit, _), ()) -> true
    | Pop_opt, Res ((Option Int, _), res) -> res = State.top_opt s
    | Top_opt, Res ((Option Int, _), res) -> res = State.top_opt s
    | Length, Res ((Int, _), res) -> res = State.length s
    | _, _ -> false
end

let () = Stm_run.run ~name:"Mutex" (module Spec) |> exit
