open Picos_std_awaitable

type 'a state =
  | Nil of { capacity : int }
  | Cons of { capacity : int; value : 'a; rest : 'a state }

let[@inline] capacity_of = function Nil r -> r.capacity | Cons r -> r.capacity

type 'a t = 'a state Awaitable.t

exception Empty

let max_capacity = Int.max_int

let create ?padded ?capacity () =
  let capacity =
    match capacity with
    | None -> max_capacity
    | Some capacity ->
        if capacity < 1 || max_capacity < capacity then invalid_arg "capacity"
        else capacity
  in
  Awaitable.make ?padded (Nil { capacity })

let rec push_await t value backoff =
  let before = Awaitable.get t in
  let capacity = capacity_of before - 1 in
  if 0 <= capacity then
    let after = Cons { capacity; value; rest = before } in
    if Awaitable.compare_and_set t before after then Awaitable.signal t
    else push_await t value (Backoff.once backoff)
  else begin
    Awaitable.await t before;
    push_await t value Backoff.default
  end

let rec push t value backoff =
  let before = Awaitable.get t in
  let capacity = capacity_of before - 1 in
  if 0 <= capacity then
    let after = Cons { capacity; value; rest = before } in
    if Awaitable.compare_and_set t before after then
      match before with Nil _ -> Awaitable.signal t | Cons _ -> ()
    else push t value (Backoff.once backoff)
  else push_await t value backoff

let rec pop_exn t backoff =
  match Awaitable.get t with
  | Nil _ -> raise_notrace Empty
  | Cons r as before ->
      if Awaitable.compare_and_set t before r.rest then begin
        let value = r.value in
        if r.capacity = 0 then Awaitable.signal t;
        value
      end
      else pop_exn t (Backoff.once backoff)

let[@inline] push t value = push t value Backoff.default
let[@inline] pop_exn t = pop_exn t Backoff.default
