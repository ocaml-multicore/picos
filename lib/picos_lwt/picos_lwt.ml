open Picos
open Lwt.Infix

let await thunk =
  let computation = Computation.create () in
  let promise =
    Lwt.try_bind thunk
      (fun value ->
        Computation.return computation value;
        Lwt.return_unit)
      (fun exn ->
        Computation.cancel computation (Exn_bt.get_callstack 0 exn);
        Lwt.return_unit)
  in
  Lwt.async (fun () -> promise);
  let trigger = Trigger.create () in
  if Computation.try_attach computation trigger then begin
    match Trigger.await trigger with
    | None -> Computation.await computation
    | Some exn_bt ->
        Lwt.cancel promise;
        Exn_bt.raise exn_bt
  end
  else Computation.await computation

let[@alert "-handler"] rec run :
    type a r.
    Fiber.t ->
    (float -> unit Lwt.t) ->
    (a, r) Effect.Shallow.continuation ->
    (a, Exn_bt.t) Result.t ->
    r Lwt.t =
 fun fiber sleep k v ->
  let effc (type a) :
      a Effect.t -> ((a, _) Effect.Shallow.continuation -> _) option = function
    | Fiber.Current -> Some (fun k -> run fiber sleep k (Ok fiber))
    | Fiber.Spawn r ->
        Some
          (fun k ->
            match Fiber.canceled fiber with
            | None ->
                let packed = Computation.Packed r.computation in
                List.iter
                  (fun main ->
                    let fiber = Fiber.create_packed ~forbid:r.forbid packed in
                    Lwt.async @@ fun () ->
                    run fiber sleep (Effect.Shallow.fiber main) (Ok ()))
                  r.mains;
                run fiber sleep k (Ok ())
            | Some exn_bt -> run fiber sleep k (Error exn_bt))
    | Fiber.Yield ->
        Some
          (fun k ->
            match Fiber.canceled fiber with
            | None -> Lwt.pause () >>= fun () -> run fiber sleep k (Ok ())
            | Some exn_bt -> run fiber sleep k (Error exn_bt))
    | Computation.Cancel_after r ->
        Some
          (fun k ->
            match Fiber.canceled fiber with
            | None ->
                let timeout =
                  sleep r.seconds >>= fun () ->
                  Computation.cancel r.computation r.exn_bt;
                  Lwt.return_unit
                in
                let canceler =
                  Trigger.from_action timeout () @@ fun _ sleep _ ->
                  Lwt.cancel sleep
                in
                if Computation.try_attach r.computation canceler then
                  Lwt.async @@ fun () -> timeout
                else Trigger.signal canceler;
                run fiber sleep k (Ok ())
            | Some exn_bt -> run fiber sleep k (Error exn_bt))
    | Trigger.Await trigger ->
        Some
          (fun k ->
            let promise, resolver = Lwt.wait () in
            let resume _trigger resolver _ = Lwt.wakeup resolver () in
            if Fiber.try_suspend fiber trigger resolver () resume then
              promise >>= fun () ->
              run fiber sleep k (Ok (Fiber.canceled fiber))
            else run fiber sleep k (Ok (Fiber.canceled fiber)))
    | _ -> None
  in
  let handler = Effect.Shallow.{ retc = Lwt.return; exnc = Lwt.fail; effc } in
  match v with
  | Ok v -> Effect.Shallow.continue_with k v handler
  | Error exn_bt -> Exn_bt.discontinue_with k exn_bt handler

let run ?(forbid = false) ~sleep main =
  let computation = Computation.create () in
  let fiber = Fiber.create ~forbid computation in
  run fiber sleep (Effect.Shallow.fiber main) (Ok ())

include Intf

module Make (Sleep : Sleep) : S = struct
  let run ?forbid main = run ?forbid ~sleep:Sleep.sleep main
  let await = await
end
