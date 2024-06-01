open Lwt.Infix

let[@inline never] not_main_thread () =
  invalid_arg "not called from the main thread"

let ready = Picos_mpscq.create ()

type pipes = {
  mutable count : int;
  mutable inn : Lwt_unix.file_descr;
  mutable out : Unix.file_descr;
  mutable close_promise : int Lwt.t;
  mutable close_resolver : int Lwt.u;
}

let pipes =
  let close_promise, close_resolver = Lwt.wait () in
  {
    count = 0;
    inn = Lwt_unix.stdin;
    out = Unix.stdout;
    close_promise;
    close_resolver;
  }

let byte = Bytes.create 1

let rec forever () =
  match Picos_mpscq.pop_exn ready with
  | resolver ->
      Lwt.wakeup resolver ();
      forever ()
  | exception Picos_mpscq.Empty ->
      let inn = pipes.inn in
      if inn == Lwt_unix.stdin then Lwt.return_unit
      else
        Lwt.pick [ pipes.close_promise; Lwt_unix.read inn byte 0 1 ]
        >>= forever_check

and forever_check n = if n < 0 then Lwt.return_unit else forever ()

module System = struct
  let sleep = Lwt_unix.sleep

  type trigger = unit Lwt.t * unit Lwt.u

  let trigger = Lwt.wait

  let signal (_, resolver) =
    if Picos_thread.is_main_thread () then Lwt.wakeup resolver ()
    else begin
      Picos_mpscq.push ready resolver;
      assert (1 = Unix.write pipes.out byte 0 1)
    end

  let await (promise, _) = promise
end

let system = (module System : Picos_lwt.System)

let pipes_incr () =
  let count = pipes.count + 1 in
  if count = 1 then begin
    let promise, resolver = Lwt.wait () in
    pipes.close_promise <- promise;
    pipes.close_resolver <- resolver;
    let inn, out = Lwt_unix.pipe_in ~cloexec:true () in
    pipes.inn <- inn;
    pipes.out <- out;
    pipes.count <- count;
    Lwt.async forever
  end
  else pipes.count <- count

let pipes_decr _ =
  let count = pipes.count - 1 in
  if count = 0 then begin
    Lwt.wakeup pipes.close_resolver (-1);
    Unix.close pipes.out;
    pipes.out <- Unix.stdout;
    Lwt.async (fun () -> Lwt_unix.close pipes.inn);
    pipes.inn <- Lwt_unix.stdin;
    pipes.count <- count
  end
  else pipes.count <- count

let run ?forbid main =
  if not (Picos_thread.is_main_thread ()) then not_main_thread ();
  pipes_incr ();
  let promise = Picos_lwt.run ?forbid system main in
  Lwt.on_any promise pipes_decr pipes_decr;
  promise

let () = Lwt_main.run (Lwt_unix.sleep 0.0)
