let[@poll error] [@inline never] compare_and_set t before after =
  !t == before
  && begin
       t := after;
       true
     end

let rec update t fn =
  let before = !t in
  let after = fn before in
  if compare_and_set t before after then before else update t fn

let[@inline] modify t fn = update t fn |> ignore
