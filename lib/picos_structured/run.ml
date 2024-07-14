open Picos

let wrap_all t main fiber =
  if Bundle.is_running t then begin
    try main () with exn -> Bundle.error t (Exn_bt.get exn)
  end;
  Fiber.finalize fiber;
  Bundle.decr t

let wrap_any t main fiber =
  if Bundle.is_running t then begin
    match main () with
    | () -> Bundle.terminate t
    | exception exn -> Bundle.error t (Exn_bt.get exn)
  end;
  Fiber.finalize fiber;
  Bundle.decr t

let rec spawn (Bundle r as t : Bundle.t) wrap = function
  | [] -> ()
  | main :: mains ->
      Bundle.unsafe_incr t;
      let fiber = Fiber.create_packed ~forbid:false r.bundle in
      Fiber.spawn fiber (wrap t main);
      spawn t wrap mains

let run actions wrap =
  Bundle.join_after @@ fun (Bundle _ as t : Bundle.t) ->
  try spawn t wrap actions
  with exn ->
    let exn_bt = Exn_bt.get exn in
    Bundle.decr t;
    Bundle.error t exn_bt

let all actions = run actions wrap_all
let any actions = run actions wrap_any
