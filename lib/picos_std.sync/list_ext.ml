let[@tail_mod_cons] rec drop_first_or_not_found x' = function
  | [] -> raise_notrace Not_found
  | x :: xs -> if x == x' then xs else x :: drop_first_or_not_found x' xs
