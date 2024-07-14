open Picos
open Lwt.Infix
include Intf

let[@inline never] not_main_thread () =
  invalid_arg "not called from the main thread"

let await promise =
  match Lwt.state promise with
  | Sleep ->
      if not (Picos_thread.is_main_thread ()) then not_main_thread ();
      let computation = Computation.create ~mode:`LIFO () in
      let promise =
        Lwt.try_bind
          (fun () -> promise)
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
  | Return value -> value
  | Fail exn -> raise exn

let[@alert "-handler"] rec go :
    type a r.
    Fiber.t ->
    (module System) ->
    (a, r) Effect.Shallow.continuation ->
    (a, Exn_bt.t) Result.t ->
    r Lwt.t =
 fun fiber ((module System) as system) k v ->
  let effc (type a) :
      a Effect.t -> ((a, _) Effect.Shallow.continuation -> _) option = function
    | Fiber.Current -> Some (fun k -> go fiber system k (Ok fiber))
    | Fiber.Spawn r ->
        Some
          (fun k ->
            match Fiber.canceled fiber with
            | None ->
                Fiber.initialize ~parent:fiber ~child:r.fiber;
                Lwt.async (fun () ->
                    go r.fiber system (Effect.Shallow.fiber r.main) (Ok r.fiber));
                go fiber system k (Ok ())
            | Some exn_bt -> go fiber system k (Error exn_bt))
    | Fiber.Yield ->
        Some
          (fun k ->
            match Fiber.canceled fiber with
            | None -> Lwt.pause () >>= fun () -> go fiber system k (Ok ())
            | Some exn_bt -> go fiber system k (Error exn_bt))
    | Computation.Cancel_after r ->
        Some
          (fun k ->
            match Fiber.canceled fiber with
            | None ->
                let timeout =
                  Lwt.try_bind
                    (fun () -> System.sleep r.seconds)
                    (fun () ->
                      Computation.cancel r.computation r.exn_bt;
                      Lwt.return_unit)
                    (function
                      | Lwt.Canceled -> Lwt.return_unit | exn -> Lwt.reraise exn)
                in
                let canceler =
                  Trigger.from_action timeout () @@ fun _ timeout _ ->
                  Lwt.cancel timeout
                in
                if Computation.try_attach r.computation canceler then
                  Lwt.async @@ fun () -> timeout
                else Trigger.signal canceler;
                go fiber system k (Ok ())
            | Some exn_bt -> go fiber system k (Error exn_bt))
    | Trigger.Await trigger ->
        Some
          (fun k ->
            let t = System.trigger () in
            if
              Fiber.try_suspend fiber trigger System.signal t
              @@ fun _ signal t -> signal t
            then
              System.await t >>= fun () ->
              go fiber system k (Ok (Fiber.canceled fiber))
            else go fiber system k (Ok (Fiber.canceled fiber)))
    | _ -> None
  in
  let handler = Effect.Shallow.{ retc = Lwt.return; exnc = Lwt.fail; effc } in
  match v with
  | Ok v -> Effect.Shallow.continue_with k v handler
  | Error exn_bt -> Exn_bt.discontinue_with k exn_bt handler

let run_fiber system fiber main =
  if not (Picos_thread.is_main_thread ()) then not_main_thread ();
  go fiber system (Effect.Shallow.fiber main) (Ok fiber)

let run ?(forbid = false) system main =
  let computation = Computation.create ~mode:`LIFO () in
  let fiber = Fiber.create ~forbid computation in
  let main fiber = Fiber.capture_and_finalize fiber computation main () in
  run_fiber system fiber main
  |> Lwt.map @@ fun () -> Computation.await computation
