type 'a node = { mutable lhs : 'a node; mutable rhs : 'a node; value : 'a }
type 'a t = 'a node

let new_node value =
  let node = { lhs = Obj.magic (); rhs = Obj.magic (); value } in
  node.lhs <- node;
  node.rhs <- node;
  node

let create () = new_node (Obj.magic ())
let is_empty t = t.lhs == t
let value node = node.value

let remove node =
  let lhs = node.lhs in
  if lhs != node then begin
    let rhs = node.rhs in
    lhs.rhs <- rhs;
    rhs.lhs <- lhs;
    node.lhs <- node;
    node.rhs <- node
  end

let move_l t node =
  let lhs = node.lhs in
  if lhs != node then begin
    let rhs = node.rhs in
    lhs.rhs <- rhs;
    rhs.lhs <- lhs
  end;
  let lhs = t.lhs in
  lhs.rhs <- node;
  node.lhs <- lhs;
  t.lhs <- node;
  node.rhs <- t

let rec iter_l action t node =
  if node != t then begin
    action node.value;
    iter_l action t node.lhs
  end

let iter_l action t = iter_l action t t.lhs
