open Picos

exception Not_a_child
exception Still_has_children

type 'a t = {
  computation : 'a Computation.t;
  finalized : Trigger.t;
  parent : Fiber.t;
}

let num_children_key =
  let finalize n = if n != 0 then raise Still_has_children in
  Fiber.FLS.new_key ~finalize (Constant 0)

let async ?give main =
  let t =
    let computation = Computation.create () in
    let finalized = Trigger.create () in
    let parent = Fiber.current () in
    { computation; finalized; parent }
  in
  let canceler =
    let (Packed from) = Fiber.get_computation t.parent in
    Computation.attach_canceler ~from ~into:t.computation (* may raise *)
  in
  Fiber.FLS.update t.parent num_children_key (( + ) 1) |> ignore;
  let child = Fiber.create ~forbid:false t.computation in
  give |> Option.iter @@ List.iter (fun node -> Ownership.own_as child node);
  let main child =
    Computation.capture t.computation main ();
    Fiber.finalize child;
    let (Packed from) = Fiber.get_computation t.parent in
    Computation.detach from canceler;
    Trigger.signal t.finalized
  in
  Fiber.spawn child main;
  t

let await t =
  if Fiber.current () != t.parent then raise Not_a_child;
  let result = Trigger.await t.finalized in
  Fiber.FLS.update t.parent num_children_key (( + ) (-1)) |> ignore;
  match result with
  | None -> Computation.await t.computation
  | Some exn_bt -> Exn_bt.raise exn_bt
