open Picos

let wrap_all t _ main _ =
  if Bundle.is_running t then begin
    try main () with exn -> Bundle.error t exn (Printexc.get_raw_backtrace ())
  end;
  Bundle.decr t

let wrap_any t _ main _ =
  if Bundle.is_running t then begin
    match main () with
    | () -> Bundle.terminate t
    | exception exn -> Bundle.error t exn (Printexc.get_raw_backtrace ())
  end;
  Bundle.decr t

let wrap_first t result main _ =
  if Bundle.is_running t then begin
    match main () with
    | value ->
        if Atomic.compare_and_set result None (Some value) then
          Bundle.terminate t
    | exception exn -> Bundle.error t exn (Printexc.get_raw_backtrace ())
  end;
  Bundle.decr t

let rec spawn (Bundle r as t : Bundle.t) state wrap = function
  | [] -> ()
  | [ main ] ->
      Bundle.unsafe_incr t;
      let unused_fake_fiber = Obj.magic () in
      wrap t state main unused_fake_fiber
  | main :: mains ->
      Bundle.unsafe_incr t;
      let fiber = Fiber.create_packed ~forbid:false r.bundle in
      (* Note that [Fiber.spawn] checks the cancelation status of the bundle. *)
      Fiber.spawn fiber (wrap t state main);
      spawn t state wrap mains

let run actions state wrap =
  Bundle.join_after @@ fun (Bundle _ as t : Bundle.t) ->
  try spawn t state wrap actions
  with exn ->
    let bt = Printexc.get_raw_backtrace () in
    Bundle.decr t;
    Bundle.error t exn bt

let all actions = run actions () wrap_all
let any actions = run actions () wrap_any

let first_or_terminate actions =
  let result = Atomic.make None in
  run actions result wrap_first;
  match Atomic.get result with
  | None -> raise Control.Terminate
  | Some value -> value
