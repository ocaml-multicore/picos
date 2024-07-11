type ('a, _) tdt =
  | Nil : ('a, [> `Nil ]) tdt
  | Cons : { value : 'a; mutable next : 'a spine } -> ('a, [> `Cons ]) tdt

and 'a spine = S : ('a, [< `Nil | `Cons ]) tdt -> 'a spine [@@unboxed]

type 'a cons = ('a, [ `Cons ]) tdt

external as_cons : 'a spine -> 'a cons = "%identity"

type ('a, _) queue =
  | Zero : ('a, [> `Zero ]) queue
  | One : {
      head : 'a cons;
      tail : 'a cons;
      cons : 'a cons;
    }
      -> ('a, [> `One ]) queue

type ('a, 'n) one = ('a, ([< `One ] as 'n)) queue
type 'a t = T : ('a, [< `Zero | `One ]) queue -> 'a t [@@unboxed]

let[@inline] singleton value =
  let cons = Cons { value; next = S Nil } in
  T (One { head = cons; tail = cons; cons })

let[@inline] exec (One o : (_, _) one) =
  if o.tail != o.cons then
    let (Cons tl) = o.tail in
    if tl.next != S o.cons then tl.next <- S o.cons

let[@inline] snoc (One o as t : (_, _) one) value =
  exec t;
  let cons = Cons { value; next = S Nil } in
  T (One { head = o.head; tail = o.cons; cons })

let[@inline] head (One { head = Cons hd; _ } : (_, _) one) = hd.value

let[@inline] tail (One o as t : (_, _) one) =
  exec t;
  if o.head == o.cons then T Zero
  else
    let (Cons hd) = o.head in
    T (One { head = as_cons hd.next; tail = o.cons; cons = o.cons })

let rec iter (Cons cons_r : _ cons) action =
  action cons_r.value;
  match cons_r.next with S Nil -> () | S (Cons _ as cons) -> iter cons action

let[@inline] iter (One o as t : (_, _) one) action =
  exec t;
  iter o.head action

let rec find_tail (Cons cons_r as cons : _ cons) =
  match cons_r.next with S Nil -> cons | S (Cons _ as cons) -> find_tail cons

let[@tail_mod_cons] rec reject (Cons cons_r : _ cons) value =
  if cons_r.value != value then
    match cons_r.next with
    | S Nil -> raise_notrace Not_found
    | S (Cons _ as cons) ->
        S (Cons { value = cons_r.value; next = reject cons value })
  else cons_r.next

let remove (One o as t : (_, _) one) value =
  exec t;
  match reject o.head value with
  | S Nil -> T Zero
  | S (Cons _ as head) ->
      let tail = find_tail head in
      T (One { head; tail; cons = tail })
  | exception Not_found -> T t
