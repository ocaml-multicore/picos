let () = Picos_select.configure ()

let benchmarks =
  [
    ("Picos Computation", Bench_computation.run_suite);
    ("Picos Current", Bench_current.run_suite);
    ("Picos FLS (excluding Current)", Bench_fls_excluding_current.run_suite);
    ("Picos TLS", Bench_tls.run_suite);
    ("Picos DLS", Bench_dls.run_suite);
    ("Picos Mutex", Bench_mutex.run_suite);
    ("Picos Spawn", Bench_spawn.run_suite);
    ("Picos Yield", Bench_yield.run_suite);
    ("Picos Cancel_after with Picos_select", Bench_cancel_after.run_suite);
    ("Ref with Picos_sync.Mutex", Bench_ref_mutex.run_suite);
    ("Picos_mpscq", Bench_mpscq.run_suite);
    ("Picos_htbl", Bench_htbl.run_suite);
    ("Picos_stdio", Bench_stdio.run_suite);
    ("Fib", Bench_fib.run_suite);
  ]

let () = Multicore_bench.Cmd.run ~benchmarks ()
