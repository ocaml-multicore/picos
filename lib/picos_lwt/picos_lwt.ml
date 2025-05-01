open Picos
open Lwt.Infix
include Intf

let[@inline never] not_main_thread () =
  invalid_arg "not called from the main thread"

let empty_bt = Printexc.get_callstack 0

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
            Computation.cancel computation exn empty_bt;
            Lwt.return_unit)
      in
      Lwt.async (fun () -> promise);
      let trigger = Trigger.create () in
      if Computation.try_attach computation trigger then begin
        match Trigger.await trigger with
        | None -> Computation.peek_exn computation
        | Some (exn, bt) ->
            Lwt.cancel promise;
            Printexc.raise_with_backtrace exn bt
      end
      else Computation.peek_exn computation
  | Return value -> value
  | Fail exn -> raise exn

let bind_on (module System : System) thunk =
  let trigger = System.trigger () in
  let promise = Lwt.bind (System.await trigger) thunk in
  System.signal trigger;
  promise

let await_on (module System : System) promise =
  let computation = Computation.create ~mode:`LIFO () in
  let trigger = System.trigger () in
  let promise =
    Lwt.bind (System.await trigger) @@ fun () ->
    Lwt.try_bind
      (fun () -> promise)
      (fun value ->
        Computation.return computation value;
        Lwt.return_unit)
      (fun exn ->
        Computation.cancel computation exn empty_bt;
        Lwt.return_unit)
  in
  System.signal trigger;
  let trigger = Trigger.create () in
  if Computation.try_attach computation trigger then begin
    match Trigger.await trigger with
    | None -> Computation.peek_exn computation
    | Some (exn, bt) ->
        Lwt.cancel promise;
        Printexc.raise_with_backtrace exn bt
  end
  else Computation.peek_exn computation

let[@alert "-handler"] rec go : type a r.
    Fiber.t ->
    (module System) ->
    (a, r) Effect.Shallow.continuation ->
    (a, exn * Printexc.raw_backtrace) Result.t ->
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
                      Computation.cancel r.computation r.exn r.bt;
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
  | Error (exn, bt) ->
      Effect.Shallow.discontinue_with_backtrace k exn bt handler

let run_fiber system fiber main =
  if not (Picos_thread.is_main_thread ()) then not_main_thread ();
  go fiber system (Effect.Shallow.fiber main) (Ok fiber)

let run ?(forbid = false) system main =
  let computation = Computation.create ~mode:`LIFO () in
  let fiber = Fiber.create ~forbid computation in
  let main _ = Computation.capture computation main () in
  run_fiber system fiber main
  |> Lwt.map @@ fun () -> Computation.peek_exn computation
