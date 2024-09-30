open Multicore_bench

let is_ocaml4 = String.starts_with ~prefix:"4." Sys.ocaml_version

external stack_size : unit -> int = "caml_picos_bench_stack_size"

let bytes_per_word = if Sys.int_size <= 32 then 4 else 8
let live_bytes () = (Gc.stat (Gc.full_major ())).live_words * bytes_per_word

let measure_live_bytes action =
  let before = live_bytes () in
  action ();
  let after = live_bytes () in
  after - before

let divide_up total divisor = Float.of_int ((total + (divisor / 2)) / divisor)

let run_suite ~budgetf:_ =
  if is_ocaml4 then []
  else
    [
      begin
        Scheduler.run @@ fun () ->
        let open Picos_std_structured in
        Bundle.join_after ~on_return:`Terminate @@ fun bundle ->
        let n = 10_000 in
        let stack = Atomic.make 0 in
        let bytes =
          measure_live_bytes @@ fun () ->
          let main () =
            Atomic.fetch_and_add stack (stack_size ()) |> ignore;
            while true do
              Control.yield ()
            done
          in
          for _ = 1 to n do
            Bundle.fork bundle main
          done;
          Control.yield ()
        in
        Metric.make ~metric:"stack and heap used" ~config:"fiber in a bundle"
          ~units:"B" ~trend:`Lower_is_better ~description:"Memory usage"
          (`Float (divide_up (bytes + Atomic.get stack) n))
      end;
      begin
        Scheduler.run @@ fun () ->
        let open Picos_std_structured in
        Bundle.join_after ~on_return:`Terminate @@ fun bundle ->
        let n = 10_000 in
        let stack = Atomic.make 0 in
        let bytes =
          measure_live_bytes @@ fun () ->
          let main () =
            Atomic.fetch_and_add stack (stack_size ()) |> ignore;
            while true do
              Control.yield ()
            done
          in
          for _ = 1 to n do
            Bundle.fork_as_promise bundle main |> ignore
          done;
          Control.yield ()
        in
        Metric.make ~metric:"stack and heap used" ~config:"promise in a bundle"
          ~units:"B" ~trend:`Lower_is_better ~description:"Memory usage"
          (`Float (divide_up (bytes + Atomic.get stack) n))
      end;
      begin
        Scheduler.run @@ fun () ->
        let open Picos_std_structured in
        Flock.join_after ~on_return:`Terminate @@ fun () ->
        let n = 10_000 in
        let stack = Atomic.make 0 in
        let bytes =
          measure_live_bytes @@ fun () ->
          let main () =
            Atomic.fetch_and_add stack (stack_size ()) |> ignore;
            while true do
              Control.yield ()
            done
          in
          for _ = 1 to n do
            Flock.fork main
          done;
          Control.yield ()
        in
        Metric.make ~metric:"stack and heap used" ~config:"fiber in a flock"
          ~units:"B" ~trend:`Lower_is_better ~description:"Memory usage"
          (`Float (divide_up (bytes + Atomic.get stack) n))
      end;
      begin
        Scheduler.run @@ fun () ->
        let open Picos_std_structured in
        Flock.join_after ~on_return:`Terminate @@ fun () ->
        let n = 10_000 in
        let stack = Atomic.make 0 in
        let bytes =
          measure_live_bytes @@ fun () ->
          let main () =
            Atomic.fetch_and_add stack (stack_size ()) |> ignore;
            while true do
              Control.yield ()
            done
          in
          for _ = 1 to n do
            Flock.fork_as_promise main |> ignore
          done;
          Control.yield ()
        in
        Metric.make ~metric:"stack and heap used" ~config:"promise in a flock"
          ~units:"B" ~trend:`Lower_is_better ~description:"Memory usage"
          (`Float (divide_up (bytes + Atomic.get stack) n))
      end;
      begin
        Scheduler.run @@ fun () ->
        let open Picos in
        let open Picos_std_sync in
        let computation = Computation.create () in
        let (Packed main) = Fiber.get_computation (Fiber.current ()) in
        let _ = Computation.attach_canceler ~from:main ~into:computation in
        let n = 10_000 in
        let stack = Atomic.make 0 in
        let latch = Latch.create 1 in
        let bytes =
          measure_live_bytes @@ fun () ->
          let main _ =
            try
              Latch.incr latch;
              Atomic.fetch_and_add stack (stack_size ()) |> ignore;
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
        Computation.cancel computation Exit (Printexc.get_callstack 0);
        Latch.decr latch;
        Latch.await latch;
        Metric.make ~metric:"stack and heap used"
          ~config:"fiber with shared computation & latch" ~units:"B"
          ~trend:`Lower_is_better ~description:"Memory usage"
          (`Float (divide_up (bytes + Atomic.get stack) n))
      end;
      begin
        Scheduler.run @@ fun () ->
        let open Picos_std_finally in
        let open Picos_std_structured in
        let n = 10_000 in
        let heap = ref 0 in
        let counter = ref n in
        let rec finalizers () =
          let@ _ = Fun.protect ~finally:Fun.id in
          decr counter;
          if 0 < !counter then finalizers ()
          else begin
            heap := live_bytes () - !heap;
            stack_size ()
          end
        in
        let stack =
          Flock.join_after @@ fun () ->
          Promise.await @@ Flock.fork_as_promise
          @@ fun () ->
          heap := live_bytes ();
          finalizers ()
        in
        Metric.make ~metric:"stack and heap used" ~config:"Fun.protect"
          ~units:"B" ~trend:`Lower_is_better ~description:"Memory usage"
          (`Float (divide_up (stack + !heap) n))
      end;
      begin
        Scheduler.run @@ fun () ->
        let open Picos_std_finally in
        let open Picos_std_structured in
        let n = 10_000 in
        let heap = ref 0 in
        let counter = ref n in
        let rec finalizers () =
          let@ _ = lastly Fun.id in
          decr counter;
          if 0 < !counter then finalizers ()
          else begin
            heap := live_bytes () - !heap;
            stack_size ()
          end
        in
        let stack =
          Flock.join_after @@ fun () ->
          Promise.await @@ Flock.fork_as_promise
          @@ fun () ->
          heap := live_bytes ();
          finalizers ()
        in
        Metric.make ~metric:"stack and heap used" ~config:"lastly" ~units:"B"
          ~trend:`Lower_is_better ~description:"Memory usage"
          (`Float (divide_up (stack + !heap) n))
      end;
      begin
        Scheduler.run @@ fun () ->
        let open Picos_std_finally in
        let open Picos_std_structured in
        let n = 10_000 in
        let heap = ref 0 in
        let counter = ref n in
        let rec finalizers () =
          let@ _ = finally Fun.id Fun.id in
          decr counter;
          if 0 < !counter then finalizers ()
          else begin
            heap := live_bytes () - !heap;
            stack_size ()
          end
        in
        let stack =
          Flock.join_after @@ fun () ->
          Promise.await @@ Flock.fork_as_promise
          @@ fun () ->
          heap := live_bytes ();
          finalizers ()
        in
        Metric.make ~metric:"stack and heap used" ~config:"finally" ~units:"B"
          ~trend:`Lower_is_better ~description:"Memory usage"
          (`Float (divide_up (stack + !heap) n))
      end;
      begin
        Scheduler.run @@ fun () ->
        let open Picos_std_finally in
        let open Picos_std_structured in
        let n = 10_000 in
        let stack = ref 0 in
        let heap = ref 0 in
        let counter = ref n in
        let rec finalizers () =
          let@ _ = instantiate Fun.id Fun.id in
          decr counter;
          if 0 < !counter then finalizers ()
          else begin
            stack := stack_size ();
            heap := live_bytes () - !heap;
            raise Control.Terminate
          end
        in
        begin
          Flock.join_after @@ fun () ->
          Flock.fork @@ fun () ->
          heap := live_bytes ();
          finalizers ()
        end;
        Metric.make ~metric:"stack and heap used" ~config:"instantiate"
          ~units:"B" ~trend:`Lower_is_better ~description:"Memory usage"
          (`Float (divide_up (!stack + !heap) n))
      end;
    ]
