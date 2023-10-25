open struct
  module Per_thread = struct
    type t = {
      mutex : Mutex.t;
      condition : Condition.t;
      fiber : [ `Sync | `Async ] Bootstrap.Fiber.t;
    }

    open struct
      let create fiber =
        let mutex = Mutex.create () and condition = Condition.create () in
        { mutex; condition; fiber }

      let per_thread_key =
        TLS.new_key @@ fun () ->
        create
          (Bootstrap.Fiber.create ~forbid:false
             (Bootstrap.Computation.create ()))
    end

    let get () = TLS.get per_thread_key
    let set fiber = TLS.set per_thread_key (create fiber)
  end

  let release _ _ (per_thread : Per_thread.t) =
    Mutex.lock per_thread.mutex;
    Mutex.unlock per_thread.mutex;
    Condition.broadcast per_thread.condition

  let block trigger (per_thread : Per_thread.t) =
    Mutex.lock per_thread.mutex;
    match
      while Atomic.get trigger != Bootstrap.Trigger.Signaled do
        Condition.wait per_thread.condition per_thread.mutex
      done
    with
    | () ->
        Mutex.unlock per_thread.mutex;
        Bootstrap.Fiber.canceled per_thread.fiber
    | exception exn ->
        (* Condition.wait may be interrupted by asynchronous exceptions and we
           must make sure to unlock even in that case. *)
        Mutex.unlock per_thread.mutex;
        raise exn
end

let await trigger =
  let per_thread = Per_thread.get () in
  if Bootstrap.Fiber.has_forbidden per_thread.fiber then
    if Bootstrap.Trigger.on_signal trigger () per_thread release then
      block trigger per_thread
    else None
  else if Bootstrap.Fiber.try_attach per_thread.fiber trigger then
    if Bootstrap.Trigger.on_signal trigger () per_thread release then
      block trigger per_thread
    else begin
      Bootstrap.Fiber.detach per_thread.fiber trigger;
      Bootstrap.Fiber.canceled per_thread.fiber
    end
  else begin
    Bootstrap.Trigger.signal trigger;
    Bootstrap.Fiber.canceled per_thread.fiber
  end

(* *)

open struct
  module Entry = struct
    type t =
      | Entry : {
          time : Mtime.span;
          exn_bt : Exn_bt.t;
          computation : 'a Bootstrap.Computation.as_cancelable;
        }
          -> t

    let compare (Entry l) (Entry r) = Mtime.Span.compare l.time r.time
  end

  module Q = Psq.Make (Int) (Entry)

  module Per_domain = struct
    type t = {
      timeouts : Q.t Atomic.t;
          (** Must be atomic to allow removal from another domain. *)
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
          timeouts = Atomic.make Q.empty;
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

    let rec add_timeout t id entry =
      let before = Atomic.get t.timeouts in
      let after = Q.add id entry before in
      if Atomic.compare_and_set t.timeouts before after then
        match Q.min after with
        | Some (id', _) -> if id = id' then wakeup t
        | None -> ()
      else add_timeout t id entry

    let rec remove_timeout _trigger t id =
      let before = Atomic.get t.timeouts in
      let after = Q.remove id before in
      if not (Atomic.compare_and_set t.timeouts before after) then
        remove_timeout (Obj.magic ()) t id

    let wait t =
      while t.state == `Locked do
        Systhreads.yield ()
      done;
      if t.state != `Alive then
        invalid_arg "Computation: domain has been terminated"

    let[@poll error] [@inline never] running_atomically t before =
      t.state == `Alive
      && Atomic.get t.timeouts == before
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
        let before = Atomic.get t.timeouts in
        match Q.pop before with
        | None -> timeout_thread_sleep t before (-1.0)
        | Some ((_, Entry e), after) ->
            let elapsed = Mtime_clock.elapsed () in
            if Mtime.Span.compare e.time elapsed <= 0 then begin
              if Atomic.compare_and_set t.timeouts before after then
                Bootstrap.Computation.cancel e.computation e.exn_bt;
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
end

let cancel_after seconds exn_bt computation =
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
          (Bootstrap.Trigger.Awaiting (Per_domain.remove_timeout, per_domain, id))
      in
      if not (Bootstrap.Computation.try_attach computation remover) then
        Bootstrap.Trigger.signal remover

(* *)

let current () =
  let fiber = (Per_thread.get ()).fiber in
  Bootstrap.Fiber.check fiber;
  fiber

let spawn forbid computation mains =
  mains
  |> List.iter @@ fun main ->
     Systhreads.create
       (fun () ->
         main (Per_thread.set (Bootstrap.Fiber.create ~forbid computation)))
       ()
     |> ignore

let yield () = Systhreads.yield ()
