open Picos

let implementation =
  (module struct
    module Per_thread = struct
      type t = { mutex : Mutex.t; condition : Condition.t; fiber : Fiber.t }

      open struct
        let create fiber =
          let mutex = Mutex.create () and condition = Condition.create () in
          { mutex; condition; fiber }

        let per_thread_key =
          Picos_tls.new_key @@ fun () ->
          create (Fiber.create ~forbid:false (Computation.create ()))
      end

      let get () = Picos_tls.get per_thread_key
      let set fiber = Picos_tls.set per_thread_key (create fiber)
    end

    module Trigger = struct
      let release _ _ (per_thread : Per_thread.t) =
        Mutex.lock per_thread.mutex;
        Mutex.unlock per_thread.mutex;
        Condition.broadcast per_thread.condition

      let block trigger (per_thread : Per_thread.t) =
        Mutex.lock per_thread.mutex;
        match
          while not (Trigger.is_signaled trigger) do
            Condition.wait per_thread.condition per_thread.mutex
          done
        with
        | () ->
            Mutex.unlock per_thread.mutex;
            Fiber.canceled per_thread.fiber
        | exception exn ->
            (* Condition.wait may be interrupted by asynchronous exceptions and we
               must make sure to unlock even in that case. *)
            Mutex.unlock per_thread.mutex;
            raise exn

      let[@alert "-handler"] await trigger =
        let per_thread = Per_thread.get () in
        if Fiber.has_forbidden per_thread.fiber then
          if Trigger.on_signal trigger () per_thread release then
            block trigger per_thread
          else None
        else if Fiber.try_attach per_thread.fiber trigger then
          if Trigger.on_signal trigger () per_thread release then
            block trigger per_thread
          else begin
            Fiber.detach per_thread.fiber trigger;
            Fiber.canceled per_thread.fiber
          end
        else begin
          Trigger.dispose trigger;
          Fiber.canceled per_thread.fiber
        end
    end

    module Computation = Select

    module Fiber = struct
      let current () =
        let fiber = (Per_thread.get ()).fiber in
        Fiber.check fiber;
        fiber

      let spawn ~forbid computation mains =
        mains
        |> List.iter @@ fun main ->
           Systhreads.create
             (fun () ->
               main (Per_thread.set (Fiber.create ~forbid computation)))
             ()
           |> ignore

      let yield () = Systhreads.yield ()
    end
  end : Implementation)
