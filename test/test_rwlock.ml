open QCheck
open STM
module Rwlock = Picos_std_sync.Rwlock

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
  type sut = { stack : int Stack.t; rwlock : Rwlock.t }

  let arb_cmd _s =
    [
      Gen.int_range 1 1000 |> Gen.map (fun x -> Push x);
      Gen.return Pop_opt;
      Gen.return Top_opt;
      Gen.return Length;
    ]
    |> Gen.oneof |> make ~print:show_cmd

  let init_state = []
  let init_sut () = { stack = Stack.create (); rwlock = Rwlock.create () }

  let next_state c s =
    match c with
    | Push x -> State.push x s
    | Pop_opt -> State.pop_opt s
    | Top_opt -> s
    | Length -> s

  let run c d =
    match c with
    | Push x ->
        Rwlock.acquire d.rwlock;
        Stack.push x d.stack;
        Rwlock.release d.rwlock;
        Res (unit, ())
    | Pop_opt ->
        Rwlock.acquire d.rwlock;
        let result = Stack.pop_opt d.stack in
        Rwlock.release d.rwlock;
        Res (option int, result)
    | Top_opt ->
        Rwlock.acquire_shared d.rwlock;
        let result = Stack.top_opt d.stack in
        Rwlock.release_shared d.rwlock;
        Res (option int, result)
    | Length ->
        Rwlock.acquire_shared d.rwlock;
        let result = Stack.length d.stack in
        Rwlock.release_shared d.rwlock;
        Res (int, result)

  let postcond c (s : state) res =
    match (c, res) with
    | Push _x, Res ((Unit, _), ()) -> true
    | Pop_opt, Res ((Option Int, _), res) -> res = State.top_opt s
    | Top_opt, Res ((Option Int, _), res) -> res = State.top_opt s
    | Length, Res ((Int, _), res) -> res = State.length s
    | _, _ -> false
end

let () = Stm_run.run ~name:"Rwlock" (module Spec) |> exit
