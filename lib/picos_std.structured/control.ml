open Picos

let[@inline never] finished () = raise (Sys_error "computation finished")
let[@inline never] forbidden () = invalid_arg "cancelation forbidden"

exception Terminate

let empty_bt = Printexc.get_callstack 0

let[@inline] get_callstack_opt = function
  | None | Some 0 -> empty_bt
  | Some n -> Printexc.get_callstack n

exception Errors of (exn * Printexc.raw_backtrace) list

let () =
  Printexc.register_printer @@ function
  | Errors exn_bts ->
      let causes =
        List.map (fun (exn, _) -> Printexc.to_string exn) exn_bts
        |> String.concat "; "
      in
      Some (Printf.sprintf "Errors[%s]" causes)
  | _ -> None

module Errors = struct
  type t = (exn * Printexc.raw_backtrace) list Atomic.t

  let create () = Atomic.make []

  let rec check (exn_bts : (exn * Printexc.raw_backtrace) list) exns =
    match exn_bts with
    | [] -> ()
    | [ ((_, bt) as exn_bt) ] ->
        Printexc.raise_with_backtrace (Errors (exn_bt :: exns)) bt
    | exn_bt :: exn_bts -> check exn_bts (exn_bt :: exns)

  let check t =
    match Atomic.get t with
    | [] -> ()
    | [ (exn, bt) ] -> Printexc.raise_with_backtrace exn bt
    | exn_bts -> check exn_bts []

  let rec push t exn bt backoff =
    let before = Atomic.get t in
    let after = (exn, bt) :: before in
    if not (Atomic.compare_and_set t before after) then
      push t exn bt (Backoff.once backoff)

  let push t exn bt = push t exn bt Backoff.default
end

let raise_if_canceled () = Fiber.check (Fiber.current ())
let yield = Fiber.yield
let sleep = Fiber.sleep

let block () =
  let fiber = Fiber.current () in
  if Fiber.has_forbidden fiber then forbidden ();
  match Trigger.await (Trigger.create ()) with
  | None -> finished ()
  | Some (exn, bt) -> Printexc.raise_with_backtrace exn bt

let protect thunk = Fiber.forbid (Fiber.current ()) thunk

let terminate_after ?callstack ~seconds thunk =
  (* The sequence of operations below ensures that nothing is leaked. *)
  let into = Computation.create ~mode:`LIFO () in
  let into_packed = Computation.Packed into in
  let fiber = Fiber.current () in
  let (Packed from as packed) = Fiber.get_computation fiber in
  let canceler = Computation.attach_canceler ~from ~into in
  (* Ideally there should be no poll point betweem [attach_canceler] and the
     [match ... with] below. *)
  Fiber.set_computation fiber into_packed;
  match
    Computation.cancel_after into ~seconds Terminate
      (get_callstack_opt callstack);
    thunk ()
  with
  | result ->
      Computation.finish into;
      let (Packed from) = packed in
      Computation.detach from canceler;
      Fiber.set_computation fiber packed;
      result
  | exception exn ->
      let bt = Printexc.get_raw_backtrace () in
      Computation.cancel into exn bt;
      let (Packed from) = packed in
      Computation.detach from canceler;
      Fiber.set_computation fiber packed;
      Printexc.raise_with_backtrace exn bt
