let in_place_shuffle array =
  let state = Random.State.make_self_init () in
  let n = Array.length array in
  for i = 0 to n - 2 do
    let j = Random.State.int state (n - i) + i in
    let t = array.(i) in
    array.(i) <- array.(j);
    array.(j) <- t
  done

let shuffle list =
  let array = Array.of_list list in
  in_place_shuffle array;
  Array.to_list array
