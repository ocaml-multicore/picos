
  type _ t = Branch : 'a Event.t * ('a -> 'res) -> 'res t

  let[@inline] make ev k = Branch (ev, k)
