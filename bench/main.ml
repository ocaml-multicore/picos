let benchmarks =
  [
    ("Picos Computation", Bench_computation.run_suite);
    ("Picos Current", Bench_current.run_suite);
    ("Picos FLS (excluding Current)", Bench_fls_excluding_current.run_suite);
    ("Picos Yield", Bench_yield.run_suite);
    ("Foundation Mpsc_queue", Bench_mpsc_queue.run_suite);
  ]

let () = Multicore_bench.Cmd.run ~benchmarks ()
