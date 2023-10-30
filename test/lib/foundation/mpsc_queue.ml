type 'a t = { tail : 'a list Atomic.t; head : 'a list ref }

let create () =
  let tail = Multicore_magic.copy_as_padded @@ Atomic.make [] in
  let head = Multicore_magic.copy_as_padded @@ ref [] in
  Multicore_magic.copy_as_padded { tail; head }

let rec enqueue backoff t x =
  let before = Atomic.get t.tail in
  let after = x :: before in
  if not (Atomic.compare_and_set t.tail before after) then
    enqueue (Backoff.once backoff) t x

let enqueue t x = enqueue Backoff.default t x

exception Empty

let dequeue t =
  match !(t.head) with
  | x :: xs ->
      t.head := xs;
      x
  | [] ->
      if Atomic.get t.tail != [] then
        match Atomic.exchange t.tail [] |> List.rev with
        | x :: xs ->
            t.head := xs;
            x
        | [] -> raise_notrace Empty
      else raise_notrace Empty
