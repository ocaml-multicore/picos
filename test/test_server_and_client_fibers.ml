let () =
  Printf.printf "Using fibers:\n%!";
  Schedulers.Fifos.run ~forbid:false Test_server_and_client.main
