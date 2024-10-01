open Picos

let[@inline never] wrap_all t main =
  match main () with
  | () -> Bundle.decr t
  | exception exn -> Bundle.raised exn t

let[@inline never] wrap_any t main =
  match main () with
  | () ->
      Bundle.terminate t;
      Bundle.decr t
  | exception exn -> Bundle.raised exn t

let rec spawn (Bundle r as t : Bundle.t) ~all = function
  | [] -> ()
  | [ main ] ->
      Bundle.unsafe_incr t;
      if Bundle.is_running t then
        if all then wrap_all t main else wrap_any t main
      else Bundle.decr t
  | main :: mains ->
      Bundle.unsafe_incr t;
      let fiber = Fiber.create_packed ~forbid:false r.bundle in
      (* Note that [Fiber.spawn] checks the cancelation status of the bundle. *)
      Fiber.spawn fiber (fun _ ->
          if Bundle.is_running t then
            if all then wrap_all t main else wrap_any t main
          else Bundle.decr t);
      spawn t ~all mains

let run actions ~all =
  Bundle.join_after @@ fun (t : Bundle.t) ->
  try spawn t ~all actions with exn -> Bundle.raised exn t

let all actions = run actions ~all:true
let any actions = run actions ~all:false
