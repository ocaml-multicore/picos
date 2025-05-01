open Picos

let[@inline never] not_main_thread () =
  invalid_arg "not called from the main thread"

module Mpscq = Picos_aux_mpscq

let ready = Mpscq.create ~padded:true ()

type notification = { mutable ref_count : int; mutable id : int }

let notification = { ref_count = 0; id = 0 }
let state = Atomic.make `Not_running

let notify_callback () =
  Atomic.set state `Running;
  let rec loop () =
    match Mpscq.pop_exn ready with
    | resolver ->
        Lwt.wakeup resolver ();
        loop ()
    | exception Mpscq.Empty -> begin
        match Atomic.get state with
        | `Not_running | `Notified ->
            Atomic.set state `Running;
            loop ()
        | `Running ->
            if not (Atomic.compare_and_set state `Running `Not_running) then
              loop ()
      end
  in
  loop ()

let rec notify () =
  match Atomic.get state with
  | `Notified -> ()
  | (`Running | `Not_running) as before ->
      if Atomic.compare_and_set state before `Notified then begin
        if before == `Not_running then
          Lwt_unix.send_notification notification.id
      end
      else notify ()

module System : Picos_lwt.System = struct
  let sleep = Lwt_unix.sleep

  type trigger = unit Lwt.t * unit Lwt.u

  let trigger = Lwt.wait

  let signal (_, resolver) =
    if Picos_thread.is_main_thread () then Lwt.wakeup resolver ()
    else begin
      Mpscq.push ready resolver;
      notify ()
    end

  let await (promise, _) = promise
end

let system = (module System : Picos_lwt.System)

let notification_decr _ =
  let ref_count = notification.ref_count - 1 in
  notification.ref_count <- ref_count;
  if ref_count = 0 then Lwt_unix.stop_notification notification.id

let run_fiber fiber main =
  if not (Picos_thread.is_main_thread ()) then not_main_thread ();
  begin
    let ref_count = notification.ref_count + 1 in
    notification.ref_count <- ref_count;
    if ref_count = 1 then
      notification.id <- Lwt_unix.make_notification notify_callback
  end;
  let promise = Picos_lwt.run_fiber system fiber main in
  Lwt.on_any promise notification_decr notification_decr;
  promise

let run ?(forbid = false) main =
  let computation = Computation.create ~mode:`LIFO () in
  let fiber = Fiber.create ~forbid computation in
  let main _ = Computation.capture computation main () in
  run_fiber fiber main |> Lwt.map @@ fun () -> Computation.peek_exn computation

let run_main ?forbid main = Lwt_main.run (run ?forbid main)
