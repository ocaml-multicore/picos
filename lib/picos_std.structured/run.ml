open Picos

let wrap_all t _ main _ =
  if Bundle.is_running t then begin
    try main () with exn -> Bundle.error t exn (Printexc.get_raw_backtrace ())
  end;
  Bundle.decr t

let wrap_any t _ main _ =
  if Bundle.is_running t then begin
    match main () with
    | () -> Bundle.terminate t
    | exception exn -> Bundle.error t exn (Printexc.get_raw_backtrace ())
  end;
  Bundle.decr t

let wrap_first t result main _ =
  if Bundle.is_running t then begin
    match main () with
    | value ->
        if Atomic.compare_and_set result None (Some value) then
          Bundle.terminate t
    | exception exn -> Bundle.error t exn (Printexc.get_raw_backtrace ())
  end;
  Bundle.decr t

let rec spawn (Bundle r as t : Bundle.t) state wrap = function
  | [] -> ()
  | [ main ] ->
      Bundle.unsafe_incr t;
      let unused_fake_fiber = Obj.magic () in
      wrap t state main unused_fake_fiber
  | main :: mains ->
      Bundle.unsafe_incr t;
      let fiber = Fiber.create_packed ~forbid:false r.bundle in
      (* Note that [Fiber.spawn] checks the cancelation status of the bundle. *)
      Fiber.spawn fiber (wrap t state main);
      spawn t state wrap mains

let run ?on_terminate actions state wrap =
  Bundle.join_after ?on_terminate @@ fun (Bundle _ as t : Bundle.t) ->
  try spawn t state wrap actions
  with exn ->
    let bt = Printexc.get_raw_backtrace () in
    Bundle.decr t;
    Bundle.error t exn bt

let all ?on_terminate actions = run ?on_terminate actions () wrap_all
let any actions = run actions () wrap_any

let first_or_terminate actions =
  let result = Atomic.make None in
  run actions result wrap_first;
  match Atomic.get result with
  | None -> raise Control.Terminate
  | Some value -> value

(* *)

let for_n = For.for_n

let find_opt_n n fn =
  let results = Atomic.make [] in
  begin
    match
      For.for_n ~on_terminate:`Raise n @@ fun i ->
      match fn i with
      | None -> ()
      | Some v ->
          let backoff = ref Backoff.default in
          while
            let before = Atomic.get results in
            let after = v :: before in
            not (Atomic.compare_and_set results before after)
          do
            backoff := Backoff.once !backoff
          done;
          raise_notrace Control.Terminate
    with
    | () -> ()
    | exception Control.Terminate -> ()
  end;
  Atomic.get results

module Array = struct
  type 'a t = 'a array

  let iter ?on_terminate action xs =
    for_n ?on_terminate (Array.length xs) @@ fun i ->
    action (Array.unsafe_get xs i)

  let[@inline never] map fn xs =
    let n = Array.length xs in
    if n = 0 then [||]
    else
      let ys = Array.make n (Obj.magic ()) in
      for_n ~on_terminate:`Raise n (fun i ->
          Array.unsafe_set ys i (fn (Array.unsafe_get xs i)));
      if Obj.double_tag != Obj.tag (Obj.repr (Array.unsafe_get ys 0)) then ys
      else Array.map Fun.id ys
end
