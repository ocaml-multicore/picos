open Picos

let key = TLS.new_key @@ fun () -> 101

let () =
  Printf.printf "Hello, from js_of_ocaml with Picos!\n%!";
  Printf.printf "Before: %d\n%!" @@ TLS.get key;
  TLS.set key 42;
  Printf.printf "After: %d\n%!" @@ TLS.get key
