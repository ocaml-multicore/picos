open Picos

let wrap_all t main =
  Bundle.unsafe_incr t;
  fun () ->
    if Bundle.is_running t then begin
      try main () with exn -> Bundle.error t (Exn_bt.get exn)
    end;
    Bundle.decr t

let wrap_any t main =
  Bundle.unsafe_incr t;
  fun () ->
    if Bundle.is_running t then begin
      try
        main ();
        Bundle.terminate t
      with exn -> Bundle.error t (Exn_bt.get exn)
    end;
    Bundle.decr t

let run actions wrap =
  Bundle.join_after @@ fun (Bundle r as t : Bundle.t) ->
  try
    let mains = List.map (wrap t) actions in
    Fiber.spawn ~forbid:false r.bundle mains
  with exn ->
    Bundle.unsafe_reset t;
    raise exn

let all actions = run actions wrap_all
let any actions = run actions wrap_any
