(** In this example, [Promise]s are implemented directly in terms of [Fiber]s
    and [Computation]s.  This perhaps has some advantages, but this also makes
    things relatively expensive as every composition spawns a new fiber (or
    more).  Also, this [Promise] is eager rather than lazy.

    ⚠️ This example is not meant to endorse this way of implementing a [Promise]
    like concept directly using [Fiber]s and [Computation]s.  This example is
    mostly meant to demonstrate that you could do so and that you can compose
    computations as demonstrated in this example. *)

open Picos

type 'a t = 'a Computation.t
type 'a unpublished = 'a Computation.t

let await = Computation.await
let of_computation = Fun.id
let peek t = if Computation.is_running t then None else Some (await t)
let try_cancel = Computation.try_cancel

module Infix = struct
  let ( let+ ) x xy =
    let y = Computation.create () in
    let main = Computation.capture y @@ fun () -> xy (await x) in
    Fiber.spawn ~forbid:false y [ main ];
    y

  let ( and+ ) x y =
    let xy = Computation.create () in
    let main =
      Computation.capture xy @@ fun () ->
      let c = Computation.canceler ~from:y ~into:xy in
      if Computation.try_attach y c then
        let x =
          match await x with
          | x ->
              Computation.detach y c;
              x
          | exception exn ->
              let bt = Printexc.get_raw_backtrace () in
              Computation.detach y c;
              Printexc.raise_with_backtrace exn bt
        in
        (x, await y)
      else
        let y = await y in
        (await x, y)
    in
    Fiber.spawn ~forbid:false xy [ main ];
    xy

  let ( let* ) x xy =
    let y = Computation.create () in
    let main = Computation.capture y @@ fun () -> await (xy (await x)) in
    Fiber.spawn ~forbid:false y [ main ];
    y

  let ( and* ) = ( and+ )
end

let both x y =
  let open Infix in
  let+ () = x and+ () = y in
  ()

let any xs =
  let y = Computation.create () in
  let main =
    Computation.capture y @@ fun () ->
    let t = Trigger.create () in
    let rec find_first_and_detach_rest = function
      | [] -> assert false
      | y :: ys ->
          if Computation.is_running y then begin
            Computation.detach y t;
            find_first_and_detach_rest ys
          end
          else begin
            List.iter (fun y -> Computation.detach y t) ys;
            Computation.await y
          end
    in
    let rec try_attach_to_all ys = function
      | [] -> begin
          match Trigger.await t with
          | Some exn_bt ->
              List.iter (fun y -> Computation.detach y t) ys;
              Exn_bt.raise exn_bt
          | None -> find_first_and_detach_rest ys
        end
      | x :: xs ->
          let ys = x :: ys in
          if Computation.try_attach x t then try_attach_to_all ys xs
          else find_first_and_detach_rest ys
    in
    try_attach_to_all [] xs
  in
  Fiber.spawn ~forbid:false y [ main ];
  y

let create () = Computation.create ()
let try_return_to = Computation.try_return
let try_reify_to t thunk = Computation.try_capture t thunk ()
let publish = of_computation
