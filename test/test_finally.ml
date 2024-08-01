open Picos_finally

let test_move_is_lazy () =
  Test_scheduler.run @@ fun () ->
  let^ moveable = finally Fun.id Fun.id in
  release moveable;
  begin
    match move moveable with _ -> () | exception _ -> assert false
  end;
  begin
    match
      let@ _ = move moveable in
      ()
    with
    | () -> assert false
    | exception Invalid_argument _ -> ()
  end

let test_borrow_returns_resource () =
  Test_scheduler.run @@ fun () ->
  let^ moveable = finally Fun.id Fun.id in
  begin
    let& _ = moveable in
    ()
  end;
  let@ _ = move moveable in
  ()

let () =
  [
    ("move", [ Alcotest.test_case "is lazy" `Quick test_move_is_lazy ]);
    ( "let&",
      [
        Alcotest.test_case "returns resource" `Quick
          test_borrow_returns_resource;
      ] );
  ]
  |> Alcotest.run "Picos_finally"
