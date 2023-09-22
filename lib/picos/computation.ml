include Bootstrap.Computation

type 'a as_cancelable = ('a, [ `Await | `Cancel ]) t
type -'allowed packed = Packed : ('a, 'allowed) t -> 'allowed packed
type packed_as_cancelable = [ `Await | `Cancel ] packed

let is_running t =
  match Atomic.get t with
  | Canceled _ | Returned _ -> false
  | Continue _ -> true

open struct
  let rec terminate backoff t after =
    match Atomic.get t with
    | Returned _ | Canceled _ -> ()
    | Continue r as before ->
        if Atomic.compare_and_set t before after then
          List.iter Trigger.signal r.triggers
        else terminate (Backoff.once backoff) t after
end

let returned_unit = Returned ()
let finished = Atomic.make returned_unit
let return t value = terminate Backoff.default t (Returned value)
let finish t = terminate Backoff.default t returned_unit
let cancel t exn_bt = terminate Backoff.default t (Canceled exn_bt)

let capture t fn x =
  match fn x with y -> return t y | exception exn -> cancel t (Exn_bt.get exn)

let rec await t =
  match Atomic.get t with
  | Returned value -> value
  | Canceled exn_bt -> Exn_bt.raise exn_bt
  | Continue _ ->
      let trigger = Trigger.create () in
      if try_attach t trigger then begin
        match Trigger.await trigger with
        | None -> await t
        | Some exn_bt ->
            detach t trigger;
            Exn_bt.raise exn_bt
      end
      else await t

let check t =
  match Atomic.get t with
  | Canceled exn_bt -> Exn_bt.raise exn_bt
  | Returned _ | Continue _ -> ()

open struct
  let propagate _ from into =
    match canceled from with None -> () | Some exn_bt -> cancel into exn_bt
end

let canceler ~from ~into =
  Atomic.make (Trigger.Awaiting (propagate, from, into))

type _ Effect.t +=
  | Cancel_after : {
      seconds : float;
      exn_bt : Exn_bt.t;
      computation : 'a as_cancelable;
    }
      -> unit Effect.t

open struct
  module Entry = struct
    type t =
      | Entry : {
          time : Mtime.span;
          exn_bt : Exn_bt.t;
          computation : 'a as_cancelable;
        }
          -> t

    let compare (Entry l) (Entry r) = Mtime.Span.compare l.time r.time
  end

  module Q = Psq.Make (Int) (Entry)

  module Per_domain = struct
    type t = {
      mutable timeouts : Q.t;
      mutable next_id : int;
      mutable needs_wakeup : bool;
      mutable state : [ `Init | `Locked | `Alive | `Dead ];
      mutable thread : Systhreads.t;
      mutable exn : exn;
      mutable pipe_inn : Select.file_descr list;
      mutable pipe_out : Select.file_descr;
      byte : Bytes.t;
    }

    open struct
      let key =
        Domain.DLS.new_key @@ fun () ->
        {
          timeouts = Q.empty;
          next_id = 0;
          needs_wakeup = true;
          state = `Init;
          thread = Systhreads.self ();
          exn = Exit;
          pipe_inn = [];
          pipe_out = Select.stdin;
          byte = Bytes.create 1;
        }
    end

    let get () = Domain.DLS.get key

    let[@poll error] [@inline never] try_lock t =
      t.state == `Init
      && begin
           t.state <- `Locked;
           true
         end

    let[@poll error] [@inline never] next_id t =
      let id = t.next_id in
      t.next_id <- id + 1;
      id

    let[@poll error] [@inline never] wakeup_needed t =
      t.needs_wakeup
      && begin
           t.needs_wakeup <- false;
           true
         end

    let wakeup t =
      let pipe_out = t.pipe_out in
      if wakeup_needed t then
        let n = Select.write pipe_out t.byte 0 1 in
        assert (n = 1)

    let[@poll error] [@inline never] cas_timeouts t before after =
      t.timeouts == before
      && begin
           t.timeouts <- after;
           true
         end

    let rec add_timeout t id entry =
      let before = t.timeouts in
      let after = Q.add id entry before in
      if cas_timeouts t before after then
        match Q.min after with
        | Some (id', _) -> if id = id' then wakeup t
        | None -> ()
      else add_timeout t id entry

    let rec remove_timeout _trigger t id =
      let before = t.timeouts in
      let after = Q.remove id before in
      if not (cas_timeouts t before after) then
        remove_timeout (Obj.magic ()) t id

    let wait t =
      while t.state == `Locked do
        Systhreads.yield ()
      done;
      if t.state != `Alive then
        invalid_arg "Computation: domain has been terminated"

    let[@poll error] [@inline never] running_atomically t before =
      t.state == `Alive && t.timeouts == before
      && begin
           t.needs_wakeup <- true;
           true
         end

    let rec timeout_thread_sleep t before next =
      if running_atomically t before then begin
        match Select.select t.pipe_inn [] [] next with
        | [ pipe_inn ], _, _ ->
            let n = Select.read pipe_inn t.byte 0 1 in
            assert (n = 1)
        | _, _, _ -> ()
      end;
      timeout_thread_handle t

    and timeout_thread_handle t =
      if t.state == `Alive then
        let before = t.timeouts in
        match Q.pop before with
        | None -> timeout_thread_sleep t before (-1.0)
        | Some ((_, Entry e), after) ->
            let elapsed = Mtime_clock.elapsed () in
            if Mtime.Span.compare e.time elapsed <= 0 then begin
              if cas_timeouts t before after then cancel e.computation e.exn_bt;
              timeout_thread_handle t
            end
            else
              let next =
                Mtime.Span.to_float_ns (Mtime.Span.abs_diff e.time elapsed)
                *. (1. /. 1_000_000_000.)
              in
              timeout_thread_sleep t before next

    let timeout_thread t =
      begin
        match timeout_thread_handle t with
        | () -> ()
        | exception exn -> t.exn <- exn
      end;
      t.needs_wakeup <- false;
      t.state <- `Dead;
      let pipe_inn = t.pipe_inn in
      t.pipe_inn <- [];
      List.iter Select.close pipe_inn;
      let pipe_out = t.pipe_out in
      t.pipe_out <- Select.stdin;
      Select.close pipe_out

    let timeout_thread t =
      t.state <- `Alive;
      timeout_thread t

    let start t =
      let pipe_inn, pipe_out = Select.pipe () in
      t.pipe_inn <- [ pipe_inn ];
      t.pipe_out <- pipe_out;
      t.thread <- Thread.create timeout_thread t;
      Domain.at_exit @@ fun () ->
      t.state <- `Dead;
      wakeup t;
      Systhreads.join t.thread;
      match t.exn with Exit -> () | exn -> raise exn

    let init t =
      if try_lock t then start t;
      wait t
  end

  let cancel_after_default seconds exn_bt computation =
    match Mtime.Span.of_float_ns (seconds *. 1_000_000_000.) with
    | None ->
        invalid_arg
          "Computation: seconds should be between 0 to pow(2, 53) nanoseconds"
    | Some span ->
        let per_domain = Per_domain.get () in
        if per_domain.state != `Alive then Per_domain.init per_domain;
        let time = Mtime.Span.add (Mtime_clock.elapsed ()) span in
        let entry = Entry.Entry { time; exn_bt; computation } in
        let id = Per_domain.next_id per_domain in
        Per_domain.add_timeout per_domain id entry;
        let remover =
          Atomic.make
            (Trigger.Awaiting (Per_domain.remove_timeout, per_domain, id))
        in
        if not (try_attach computation remover) then Trigger.signal remover
end

let cancel_after computation ~seconds exn_bt =
  if seconds < 0.0 then invalid_arg "Computation: negative seconds"
  else
    try Effect.perform (Cancel_after { seconds; exn_bt; computation })
    with Effect.Unhandled (Cancel_after { seconds; exn_bt; computation }) ->
      cancel_after_default seconds exn_bt computation
