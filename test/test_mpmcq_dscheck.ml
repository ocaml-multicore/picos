module Atomic = Dscheck.TracedAtomic
module Q = Picos_aux_mpmcq

let popped_some q =
  let b = match Q.pop_exn q with _ -> true | exception Q.Empty -> false in
  Atomic.check (fun () -> b)

let test_multi_push_pop () =
  Atomic.trace @@ fun () ->
  let q = Q.create () in
  Q.push q 764;
  Q.push q 203;
  begin
    Atomic.spawn @@ fun () ->
    Q.push q 300;
    popped_some q;
    Q.push_head q 107;
    Q.push q 395
  end;
  begin
    Atomic.spawn @@ fun () ->
    popped_some q;
    popped_some q;
    Q.push q 626
    (*Q.push q 355;*)
    (*Q.push_head q 892*)
  end;
  Atomic.final @@ fun () ->
  for _ = 1 to 3 do
    popped_some q
  done

let () =
  Alcotest.run "Picos_mpmcq DSCheck"
    [
      ( "Multiple pushes and pops",
        [ Alcotest.test_case "" `Quick test_multi_push_pop ] );
    ]
