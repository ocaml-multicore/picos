open Multicore_bench

let is_ocaml4 = String.starts_with ~prefix:"4." Sys.ocaml_version
let bytes_per_word = if Sys.int_size <= 32 then 4 else 8

let measure_live_bytes action =
  let before = Gc.stat (Gc.full_major ()) in
  action ();
  let after = Gc.stat (Gc.full_major ()) in
  (after.live_words - before.live_words) * bytes_per_word

let run_suite ~budgetf:_ =
  if is_ocaml4 then []
  else
    [
      begin
        Scheduler.run @@ fun () ->
        let open Picos_structured in
        Bundle.join_after ~on_return:`Terminate @@ fun bundle ->
        let n = 10_000 in
        let bytes =
          measure_live_bytes @@ fun () ->
          let main () =
            while true do
              Control.yield ()
            done
          in
          for _ = 1 to n do
            Bundle.fork bundle main
          done;
          Control.yield ()
        in
        Metric.make ~metric:"memory used" ~config:"fiber in a bundle" ~units:"B"
          ~trend:`Lower_is_better ~description:"Memory usage"
          (`Float (Float.of_int (bytes / n)))
      end;
      begin
        Scheduler.run @@ fun () ->
        let open Picos_structured in
        Bundle.join_after ~on_return:`Terminate @@ fun bundle ->
        let n = 10_000 in
        let bytes =
          measure_live_bytes @@ fun () ->
          let main () =
            while true do
              Control.yield ()
            done
          in
          for _ = 1 to n do
            Bundle.fork_as_promise bundle main |> ignore
          done;
          Control.yield ()
        in
        Metric.make ~metric:"memory used" ~config:"promise in a bundle"
          ~units:"B" ~trend:`Lower_is_better ~description:"Memory usage"
          (`Float (Float.of_int (bytes / n)))
      end;
      begin
        Scheduler.run @@ fun () ->
        let open Picos_structured in
        Flock.join_after ~on_return:`Terminate @@ fun () ->
        let n = 10_000 in
        let bytes =
          measure_live_bytes @@ fun () ->
          let main () =
            while true do
              Control.yield ()
            done
          in
          for _ = 1 to n do
            Flock.fork main
          done;
          Control.yield ()
        in
        Metric.make ~metric:"memory used" ~config:"fiber in a flock" ~units:"B"
          ~trend:`Lower_is_better ~description:"Memory usage"
          (`Float (Float.of_int (bytes / n)))
      end;
      begin
        Scheduler.run @@ fun () ->
        let open Picos_structured in
        Flock.join_after ~on_return:`Terminate @@ fun () ->
        let n = 10_000 in
        let bytes =
          measure_live_bytes @@ fun () ->
          let main () =
            while true do
              Control.yield ()
            done
          in
          for _ = 1 to n do
            Flock.fork_as_promise main |> ignore
          done;
          Control.yield ()
        in
        Metric.make ~metric:"memory used" ~config:"promise in a flock"
          ~units:"B" ~trend:`Lower_is_better ~description:"Memory usage"
          (`Float (Float.of_int (bytes / n)))
      end;
      begin
        Scheduler.run @@ fun () ->
        let open Picos in
        let open Picos_sync in
        let computation = Computation.create () in
        let (Packed main) = Fiber.get_computation (Fiber.current ()) in
        let _ = Computation.attach_canceler ~from:main ~into:computation in
        let n = 10_000 in
        let latch = Latch.create 1 in
        let bytes =
          measure_live_bytes @@ fun () ->
          let main _ =
            try
              Latch.incr latch;
              while true do
                Fiber.yield ()
              done
            with Exit -> Latch.decr latch
          in
          for _ = 1 to n do
            Fiber.spawn (Fiber.create ~forbid:false computation) main
          done;
          Fiber.yield ()
        in
        Computation.cancel computation (Exn_bt.get_callstack 0 Exit);
        Latch.decr latch;
        Latch.await latch;
        Metric.make ~metric:"memory used"
          ~config:"fiber with shared computation & latch" ~units:"B"
          ~trend:`Lower_is_better ~description:"Memory usage"
          (`Float (Float.of_int (bytes / n)))
      end;
    ]
