let benchmarks =
  [
    ("Picos Current", Bench_current.run_suite);
    ("Foundation Mpsc_queue", Bench_mpsc_queue.run_suite);
  ]

let () = Multicore_bench.Cmd.run ~benchmarks ()
