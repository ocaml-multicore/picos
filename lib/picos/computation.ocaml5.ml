type _ Effect.t +=
  | Cancel_after : {
      seconds : float;
      exn : exn;
      bt : Printexc.raw_backtrace;
      computation : 'a t;
    }
      -> unit Effect.t

let cancel_after computation ~seconds exn bt =
  check_non_negative seconds;
  Effect.perform (Cancel_after { seconds; exn; bt; computation })
