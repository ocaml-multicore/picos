let finally release acquire = (release, acquire)

let[@inline never] ( let@ ) (release, acquire) body =
  let x = acquire () in
  match body x with
  | y ->
      release x;
      y
  | exception exn ->
      release x;
      raise exn
