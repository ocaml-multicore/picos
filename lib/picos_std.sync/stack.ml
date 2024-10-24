open Picos_std_awaitable

type 'a state =
  | Nil of { mutable capacity : int }
  | Cons of { mutable capacity : int; value : 'a; rest : 'a state }

type 'a t = 'a state Awaitable.t

exception Empty

let busy_bit = 0b01
let one = 0b10
let max_capacity = Int.max_int / one

let create ?padded ?capacity () =
  let capacity =
    match capacity with
    | None -> max_capacity * one
    | Some capacity ->
        if capacity < 1 || max_capacity < capacity then invalid_arg "capacity"
        else capacity * one
  in
  Awaitable.make ?padded (Nil { capacity })

let rec push t value backoff =
  match Awaitable.get t with
  | Nil r as before ->
      let capacity = r.capacity land lnot busy_bit in
      if
        Awaitable.compare_and_set t before
          (Cons { capacity = capacity - one; value; rest = Nil { capacity } })
      then begin
        if r.capacity land busy_bit <> 0 then Awaitable.broadcast t
      end
      else push t value (Backoff.once backoff)
  | Cons r as before ->
      let capacity = r.capacity in
      if one <= capacity then begin
        if
          not
            (Awaitable.compare_and_set t before
               (Cons { capacity = capacity - one; value; rest = before }))
        then push t value (Backoff.once backoff)
      end
      else begin
        if capacity <> capacity lor busy_bit then
          r.capacity <- capacity lor busy_bit;
        Awaitable.await t before;
        push t value Backoff.default
      end

let rec pop_exn t backoff =
  match Awaitable.get t with
  | Nil _ -> raise_notrace Empty
  | Cons r as before ->
      if Awaitable.compare_and_set t before r.rest then begin
        if r.capacity land busy_bit <> 0 then Awaitable.broadcast t;
        r.value
      end
      else pop_exn t (Backoff.once backoff)

let[@inline] push t value = push t value Backoff.default
let[@inline] pop_exn t = pop_exn t Backoff.default
