type 'a t = { tail : 'a list Atomic.t; head : 'a list ref }

let create () =
  let tail = Multicore_magic.copy_as_padded @@ Atomic.make [] in
  let head = Multicore_magic.copy_as_padded @@ ref [] in
  Multicore_magic.copy_as_padded { tail; head }

let rec enqueue t x backoff =
  let before = Atomic.get t.tail in
  let after = x :: before in
  if not (Atomic.compare_and_set t.tail before after) then
    enqueue t x (Backoff.once backoff)

let enqueue t x = enqueue t x Backoff.default

exception Empty

let dequeue t =
  match !(t.head) with
  | x :: xs ->
      t.head := xs;
      x
  | [] -> begin
      match Atomic.exchange t.tail [] with
      | [] -> raise_notrace Empty
      | [ x ] -> x
      | x :: xs -> begin
          match List.rev_append xs [ x ] with
          | x :: xs ->
              t.head := xs;
              x
          | [] -> raise_notrace Empty
        end
    end
