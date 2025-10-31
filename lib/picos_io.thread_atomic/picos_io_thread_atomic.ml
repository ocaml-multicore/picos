let[@poll error] [@inline never] compare_and_set x before after =
  !x == before
  && begin
    x := after;
    true
  end

let[@poll error] [@inline never] exchange x after =
  let before = !x in
  x := after;
  before
