open Picos

exception Terminate

let terminate_bt = Exn_bt.get_callstack 0 Terminate

let terminate_bt ?callstack () =
  match callstack with
  | None -> terminate_bt
  | Some n -> Exn_bt.get_callstack n Terminate

exception Errors of Exn_bt.t list

let () =
  Printexc.register_printer @@ function
  | Errors exn_bts ->
      let causes =
        List.map (fun exn_bt -> Printexc.to_string exn_bt.Exn_bt.exn) exn_bts
        |> String.concat "; "
      in
      Some (Printf.sprintf "Errors[%s]" causes)
  | _ -> None

module Errors = struct
  type t = Exn_bt.t list Atomic.t

  let create () = Atomic.make []

  let rec check (exn_bts : Exn_bt.t list) exns =
    match exn_bts with
    | [] -> ()
    | [ exn_bt ] ->
        Printexc.raise_with_backtrace (Errors (exn_bt :: exns)) exn_bt.bt
    | exn_bt :: exn_bts -> check exn_bts (exn_bt :: exns)

  let check t =
    match Atomic.get t with
    | [] -> ()
    | [ exn_bt ] -> Exn_bt.raise exn_bt
    | exn_bts -> check exn_bts []

  let rec push t exn_bt backoff =
    let before = Atomic.get t in
    let after = exn_bt :: before in
    if not (Atomic.compare_and_set t before after) then
      push t exn_bt (Backoff.once backoff)

  let push t (exn_bt : Exn_bt.t) =
    if exn_bt.exn != Terminate then push t exn_bt Backoff.default
end

let raise_if_canceled () = Fiber.check (Fiber.current ())
let yield = Fiber.yield
let sleep = Fiber.sleep

let block () =
  match Trigger.await (Trigger.create ()) with
  | None -> failwith "impossible"
  | Some exn_bt -> Exn_bt.raise exn_bt

let protect thunk = Fiber.forbid (Fiber.current ()) thunk
