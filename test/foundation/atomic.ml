include Stdlib.Atomic

let rec update t fn =
  let before = Stdlib.Atomic.get t in
  let after = fn before in
  if Stdlib.Atomic.compare_and_set t before after then before else update t fn

let modify t fn = update t fn |> ignore
