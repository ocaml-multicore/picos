open Multicore_bench

module Key = struct
  type t = int

  let equal = Int.equal
  let hash = Fun.id
end

module Htbl : sig
  type 'v t

  val create : unit -> 'v t
  val clear : 'v t -> unit
  val find_opt : 'v t -> Key.t -> 'v option
  val replace : 'v t -> Key.t -> 'v -> unit
  val remove : 'v t -> Key.t -> unit
end = struct
  open Picos_std_sync
  module Htbl = Hashtbl.Make (Key)

  type 'v t = { htbl : 'v Htbl.t; rwlock : Rwlock.t }

  let create () =
    { htbl = Htbl.create 1000; rwlock = Rwlock.create ~padded:true () }

  let clear t =
    Rwlock.lock t.rwlock;
    Htbl.clear t.htbl;
    Rwlock.unlock t.rwlock

  let find_opt t key =
    Rwlock.lock_ro t.rwlock;
    let result = Htbl.find_opt t.htbl key in
    Rwlock.unlock t.rwlock;
    result

  let replace t key value =
    Rwlock.lock t.rwlock;
    Htbl.replace t.htbl key value;
    Rwlock.unlock t.rwlock

  let remove t key =
    Rwlock.lock t.rwlock;
    Htbl.remove t.htbl key;
    Rwlock.unlock t.rwlock
end

let run_one ~budgetf ~n_domains ?(n_ops = 100 * Util.iter_factor)
    ?(n_keys = 1000) ~percent_mem ?(percent_add = (100 - percent_mem + 1) / 2)
    ?(prepopulate = true) () =
  let limit_mem = percent_mem in
  let limit_add = percent_mem + percent_add in

  assert (0 <= limit_mem && limit_mem <= 100);
  assert (limit_mem <= limit_add && limit_add <= 100);

  let t = Htbl.create () in

  let n_ops = (100 + percent_mem) * n_ops / 100 in
  let n_ops = n_ops * n_domains in

  let n_ops_todo = Countdown.create ~n_domains () in

  let before () =
    Htbl.clear t;
    if prepopulate then begin
      for _ = 1 to n_keys do
        let value = Random.bits () in
        let key = value mod n_keys in
        Htbl.replace t key value
      done
    end;
    Countdown.non_atomic_set n_ops_todo n_ops
  in
  let init _ = Scheduler.run @@ fun () -> Random.State.make_self_init () in
  let wrap _ _ = Scheduler.run in
  let work domain_index state =
    let rec work () =
      let n = Countdown.alloc n_ops_todo ~domain_index ~batch:1000 in
      if n <> 0 then begin
        for _ = 1 to n do
          let value = Random.State.bits state in
          let op = (value asr 20) mod 100 in
          let key = value mod n_keys in
          if op < percent_mem then Htbl.find_opt t key |> ignore
          else if op < limit_add then Htbl.replace t key value
          else Htbl.remove t key
        done;
        work ()
      end
    in
    work ()
  in

  let config =
    Printf.sprintf "%d worker%s, %d%% reads" n_domains
      (if n_domains = 1 then "" else "s")
      percent_mem
  in
  Times.record ~budgetf ~n_domains ~n_warmups:1 ~n_runs_min:1 ~before ~init
    ~wrap ~work ()
  |> Times.to_thruput_metrics ~n:n_ops ~singular:"operation" ~config

let run_suite ~budgetf =
  Util.cross [ 1; 2; 4; 8 ] [ 10; 50; 90; 95; 100 ]
  |> List.concat_map @@ fun (n_domains, percent_mem) ->
     if Picos_domain.recommended_domain_count () < n_domains then []
     else run_one ~budgetf ~n_domains ~percent_mem ()
