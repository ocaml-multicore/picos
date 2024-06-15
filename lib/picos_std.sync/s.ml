type ('a, _) tdt =
  | Nil : ('a, [> `Nil ]) tdt
  | Cons : { value : 'a; mutable next : 'a t } -> ('a, [> `Cons ]) tdt

and 'a t = T : ('a, [< `Nil | `Cons ]) tdt -> 'a t [@@unboxed]

type 'a cons = ('a, [ `Cons ]) tdt

external as_cons : 'a t -> 'a cons = "%identity"

let[@inline] value (Cons cons_r : _ cons) = cons_r.value
let[@inline] next_as_cons (Cons cons_r : _ cons) = as_cons cons_r.next

let[@inline] exec (tail : _ cons) (cons : _ cons) =
  if tail != cons then
    let (Cons tl) = tail in
    if tl.next != T cons then tl.next <- T cons

let[@inline] cons value next = T (Cons { value; next })

let rec reverse_to tail = function
  | T Nil -> tail
  | T (Cons cons_r as next) ->
      let prev = cons_r.next in
      cons_r.next <- T tail;
      reverse_to next prev

let rec iter action (Cons head_r as head : _ cons) tail =
  action head_r.value;
  if head != tail then iter action (as_cons head_r.next) tail

let[@tail_mod_cons] rec reject (Cons cons_r : _ cons) value =
  if cons_r.value != value then
    match cons_r.next with
    | T Nil -> raise_notrace Not_found
    | T (Cons _ as cons) ->
        T (Cons { value = cons_r.value; next = reject cons value })
  else cons_r.next

let rec find_tail (Cons cons_r as cons : _ cons) =
  match cons_r.next with T Nil -> cons | T (Cons _ as cons) -> find_tail cons
