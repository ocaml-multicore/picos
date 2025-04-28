(** This example is inspired by the Clerical language:

    https://github.com/andrejbauer/clerical/tree/cooperative-threads *)

open Picos_std_structured

(** Examples from Clerical:

    https://github.com/andrejbauer/clerical/blob/ea2d3b41c942a9138aa65c329ae67c6e170e1dd3/examples/guards.real#L1-L44
*)

let guard1 () =
  Run.first_or_terminate
    [
      (fun () ->
        Control.terminate_unless (0.0 < 0.0);
        13);
      (fun () ->
        Control.terminate_unless (0.0 < 0.00000000000000000000000000000001);
        42);
    ]

let guard2 () =
  Run.first_or_terminate
    [
      (fun () ->
        Control.terminate_unless
          (while true do
             Control.yield ()
           done;
           true);
        13);
      (fun () ->
        Control.terminate_unless true;
        42);
    ]

let guard3 () =
  Run.first_or_terminate
    [
      (fun () ->
        Control.terminate_unless
          (let i = ref 0 in
           while !i < 10000 do
             i := !i + 1;
             (* Fibers are cooperative, so we should yield. *)
             Control.yield ()
           done;
           true);
        42);
    ]

(** This example is actually not guaranteed to return 42. The result depends on
    scheduling. The original example is incorrect. *)
let guard4 () =
  Run.first_or_terminate
    [
      (fun () ->
        Control.terminate_unless
          (let i = ref 0 in
           while !i < 10000 do
             i := !i + 1;
             (* Fibers are cooperative, so we should yield.  This also makes the
                result 42 more likely when running fibers in domains or
                threads. *)
             Control.yield ()
           done;
           true);
        13);
      (fun () ->
        Control.terminate_unless true;
        42);
    ]

let guard5 () =
  Run.first_or_terminate
    [
      (fun () ->
        Control.terminate_unless true;
        Run.first_or_terminate
          [
            (fun () ->
              Control.terminate_unless
                (while true do
                   Control.yield ()
                 done;
                 true);
              13);
          ]);
      (fun () ->
        Control.terminate_unless true;
        42);
    ]

let f b n =
  Run.first_or_terminate
    [
      (fun () ->
        Control.terminate_unless
          (let i = ref 0 in
           while !i < n do
             i := !i + 1;
             Control.yield ()
           done;
           true);
        b);
      (fun () ->
        Control.terminate_unless
          (let j = ref 0 in
           while !j < n do
             j := !j + 1;
             Control.yield ()
           done;
           false);
        false);
    ]

let demo () =
  Run.first_or_terminate
    [
      (fun () ->
        Control.terminate_unless (f false 1000);
        17);
      (fun () ->
        Control.terminate_unless (f false 80000);
        11);
      (fun () ->
        Control.terminate_unless
          (while true do
             Control.yield ()
           done;
           true);
        27);
      (fun () ->
        Control.terminate_unless
          (Run.first_or_terminate
             [
               (fun () ->
                 Control.terminate_unless (f false 100000);
                 false);
               (fun () ->
                 Control.terminate_unless (f true 200000);
                 true);
             ]);
        42);
      (fun () ->
        Control.terminate_unless
          (Run.first_or_terminate
             [
               (fun () ->
                 Control.terminate_unless (f true 100);
                 false);
               (fun () ->
                 Control.terminate_unless (f false 10);
                 false);
             ]);
        13);
    ]

let () =
  (* We use the test scheduler here, but in more practical use cases one should
     likely use e.g. the multififo or the random scheduler to compute with
     guarded case statements. *)
  let max_domains = Picos_domain.recommended_domain_count () in
  Test_scheduler.run ~verbose:true ~max_domains @@ fun () ->
  Run.all
    [
      (fun () -> assert (42 = guard1 ()));
      (fun () -> assert (42 = guard2 ()));
      (fun () -> assert (42 = guard3 ()));
      (fun () ->
        if 42 <> guard4 () then
          print_endline "Guard 4 wasn't 42, but that is a known issue.");
      (fun () -> assert (42 = guard5 ()));
      (fun () -> assert (42 = demo ()));
    ];
  print_endline "Ran guarded case statement examples."
