module Atomic = Multicore_magic.Transparent_atomic

type 'a node = { mutable next : 'a node; index : int; mutable value : 'a }
type 'a t = { head : 'a node Atomic.t; tail : 'a node Atomic.t }

exception Empty

let[@inline] create_sentinel index =
  let sentinel = { next = Obj.magic index; index; value = Obj.magic index } in
  sentinel.next <- sentinel;
  sentinel

let[@inline] maybe_fix tail =
  let mystery = tail.next in
  if mystery.index = tail.index - 1 then
    let prev = mystery in
    if prev.next != tail then prev.next <- tail

let create ?padded () =
  let sentinel = create_sentinel 0 in
  let head = Atomic.make sentinel |> Multicore_magic.copy_as ?padded in
  let tail = Atomic.make sentinel |> Multicore_magic.copy_as ?padded in
  { head; tail } |> Multicore_magic.copy_as ?padded

let rec push tail_atomic value backoff =
  let tail = Atomic.get tail_atomic in
  let new_tail = { next = tail; index = tail.index + 1; value } in
  maybe_fix tail;
  if Atomic.compare_and_set tail_atomic tail new_tail then tail.next <- new_tail
  else push tail_atomic value (Backoff.once backoff)

let rec pop_exn t backoff =
  let head_atomic = t.head in
  let head = Atomic.get head_atomic in
  let next = head.next in
  if head.index + 1 = next.index then
    if Atomic.compare_and_set head_atomic head next then begin
      let value = next.value in
      next.value <- Obj.magic ();
      value
    end
    else pop_exn t (Backoff.once backoff)
  else
    let tail = Atomic.get t.tail in
    if tail == head then raise_notrace Empty
    else begin
      maybe_fix tail;
      pop_exn t Backoff.default
    end

let rec push_head t value backoff =
  let head = Atomic.get t.head in
  let next = head.next in
  let index = head.index in
  if index + 1 = next.index then begin
    let new_next = { next; index; value } in
    let index = index - 1 in
    let new_head = { next = new_next; index; value = Obj.magic index } in
    if not (Atomic.compare_and_set t.head head new_head) then
      push_head t value (Backoff.once backoff)
  end
  else
    let tail = Atomic.get t.tail in
    if tail == head then
      let new_tail = { next = tail; index = tail.index + 1; value } in
      if Atomic.compare_and_set t.tail tail new_tail then tail.next <- new_tail
      else push_head t value (Backoff.once backoff)
    else begin
      maybe_fix tail;
      push_head t value Backoff.default
    end

let rec length t =
  let tail_atomic = t.tail in
  let head_atomic = t.head in
  let tail = Atomic.get tail_atomic in
  let head = Atomic.fenceless_get head_atomic in
  if tail == Atomic.get tail_atomic then tail.index - head.index else length t

type ('a, _) res = Seq : ('a, 'a Seq.t) res | Array : ('a, 'a array) res

let rec pop_all_as : type a r. a t -> (a, r) res -> _ -> r =
 fun t result backoff ->
  let head = Atomic.get t.head in
  let next = head.next in
  if head.index + 1 = next.index then begin
    let new_sentinel = create_sentinel head.index in
    if Atomic.compare_and_set t.head head new_sentinel then begin
      (* TODO: not lock-free. *)
      let tail = Atomic.exchange t.tail new_sentinel in
      maybe_fix tail;
      match result with
      | Seq ->
          let rec to_seq work tail () =
            Seq.Cons
              ( work.value,
                if work == tail then Seq.empty else to_seq work.next tail )
          in
          to_seq head.next tail
      | Array ->
          let n = tail.index - head.index in
          let work = ref head.next in
          Array.init n @@ fun _ ->
          let node = !work in
          work := node.next;
          node.value
    end
    else pop_all_as t result (Backoff.once backoff)
  end
  else
    let tail = Atomic.get t.tail in
    if tail == head then match result with Seq -> Seq.empty | Array -> [||]
    else begin
      maybe_fix tail;
      pop_all_as t result Backoff.default
    end

let[@inline] push t value = push t.tail value Backoff.default
let[@inline] pop_exn t = pop_exn t Backoff.default
let[@inline] pop_all t = pop_all_as t Seq Backoff.default
let[@inline] pop_all_as_array t = pop_all_as t Array Backoff.default
let[@inline] push_head t value = push_head t value Backoff.default
let[@inline] is_empty t = length t == 0
