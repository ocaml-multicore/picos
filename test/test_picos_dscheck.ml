(** These tests are using DSCheck, which basically goes through all the possible
    interleavings of [Atomic] operations performed by different [Atomic.spawn]ed
    fibers. *)

open Picos_bootstrap

let sum_as fn xs = Array.fold_left (fun sum x -> sum + fn x) 0 xs
let ( += ) x y = x := !x + y

(** This tries to cover most of the contract of [Trigger]s. *)
let test_trigger_contract () =
  let signaled_total = ref 0
  and won_total = ref 0
  and lost_total = ref 0
  and raised_total = ref 0 in
  let () =
    Atomic.trace @@ fun () ->
    let trigger = Trigger.create () in
    let () = Atomic.check @@ fun () -> Trigger.is_initial trigger in
    let signaled = ref 0 and won = ref 0 and lost = ref 0 and raised = ref 0 in
    for _ = 1 to 2 do
      Atomic.spawn @@ fun () -> Trigger.signal trigger
    done;
    for _ = 1 to 2 do
      Atomic.spawn @@ fun () ->
      match Trigger.on_signal trigger `X `Y (fun _ `X `Y -> incr signaled) with
      | success -> if success then incr won else incr lost
      | exception Invalid_argument _ -> incr raised
    done;
    Atomic.final @@ fun () ->
    Atomic.check @@ fun () ->
    signaled_total += !signaled;
    won_total += !won;
    lost_total += !lost;
    raised_total += !raised;
    Trigger.is_signaled trigger
    && !won <= 1 && !won = !signaled && !lost <= 2 && !raised <= !won
    && !raised = Bool.to_int (!lost = 0)
    && !won + !lost + !raised = 2
  in
  [ signaled_total; won_total; lost_total; raised_total ]
  |> List.iter @@ fun total -> if !total = 0 then Alcotest.fail "uncovered case"

(** This tries to cover much of the public contract of [Computation]s. *)
let test_computation_contract () =
  [ `FIFO; `LIFO ]
  |> List.iter @@ fun mode ->
     let attached_total = ref 0 and unattached_total = ref 0 in
     let () =
       Atomic.trace @@ fun () ->
       let computation = Computation.create ~mode () in
       let returns = ref 0 and cancels = ref 0 in
       let () =
         Atomic.spawn @@ fun () ->
         if Computation.try_return computation 101 then incr returns
       in
       let () =
         Atomic.spawn @@ fun () ->
         if Computation.try_cancel computation Exit (Printexc.get_callstack 1)
         then incr cancels
       in
       let triggers = Array.init 2 @@ fun _ -> Trigger.create () in
       let attached = ref 0 and unattached = ref 0 in
       let () =
         triggers
         |> Array.iter @@ fun trigger ->
            Atomic.spawn @@ fun () ->
            if Computation.try_attach computation trigger then incr attached
            else incr unattached
       in
       Atomic.final @@ fun () ->
       Atomic.check @@ fun () ->
       attached_total += !attached;
       unattached_total += !unattached;
       begin
         match Computation.peek computation with
         | Some (Ok 101) when !returns = 1 && !cancels = 0 -> true
         | Some (Error (Exit, _)) when !returns = 0 && !cancels = 1 -> true
         | _ -> false
       end
       && !attached + !unattached = Array.length triggers
       && !attached
          = sum_as
              (fun trigger -> Bool.to_int (Trigger.is_signaled trigger))
              triggers
     in
     [ attached_total; unattached_total ]
     |> List.iter @@ fun total ->
        if !total = 0 then Alcotest.fail "uncovered case"

(** This covers the contract of [Computation] to remove detached triggers.

    Testing this through the public API would require relying on GC
    statistics. *)
let test_computation_removes_triggers () =
  [ `FIFO; `LIFO ]
  |> List.iter @@ fun mode ->
     Atomic.trace @@ fun () ->
     let computation = Computation.create ~mode () in
     let triggers = Array.init 4 @@ fun _ -> Trigger.create () in
     let () =
       triggers
       |> Array.iter @@ fun trigger ->
          Atomic.spawn @@ fun () ->
          Atomic.check (fun () -> Computation.try_attach computation trigger);
          Computation.detach computation trigger
     in
     Atomic.final @@ fun () ->
     Atomic.check @@ fun () ->
     Array.for_all Trigger.is_signaled triggers
     &&
     match Atomic.get computation with
     | S (Canceled _) | S (Returned _) -> false
     | S (Continue { balance_and_mode; triggers }) ->
         balance_and_mode <= Computation.fifo_bit
         && List.length triggers <= 2
         &&
         let trigger = Trigger.create () in
         Computation.try_attach computation trigger
         && begin
              match Atomic.get computation with
              | S (Canceled _) | S (Returned _) -> false
              | S (Continue { balance_and_mode; triggers }) ->
                  balance_and_mode <= Computation.one + Computation.fifo_bit
                  && triggers = [ trigger ]
            end

let () =
  Alcotest.run "Picos DSCheck"
    [
      ( "Trigger",
        [ Alcotest.test_case "basic contract" `Quick test_trigger_contract ] );
      ( "Computation",
        [
          Alcotest.test_case "basic contract" `Quick test_computation_contract;
          Alcotest.test_case "removes triggers" `Quick
            test_computation_removes_triggers;
        ] );
    ]
