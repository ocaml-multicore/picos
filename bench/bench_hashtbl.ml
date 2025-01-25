open Multicore_bench

module Key = struct
  type t = int

  let equal = Int.equal
  let hash = Fun.id
end

module Hashtbl = Hashtbl.Make (Key)

module Hashtbl_lock : sig
  type 'v t

  val create : unit -> 'v t
  val clear : 'v t -> unit
  val find_opt : 'v t -> Key.t -> 'v option
  val replace : 'v t -> Key.t -> 'v -> unit
  val remove : 'v t -> Key.t -> unit
end = struct
  open Picos_std_sync

  type 'v t = { htbl : 'v Hashtbl.t; lock : Lock.t }

  let create () =
    { htbl = Hashtbl.create 1000; lock = Lock.create ~padded:true () }
    |> Multicore_magic.copy_as_padded

  let clear t =
    Lock.acquire t.lock;
    Hashtbl.clear t.htbl;
    Lock.release t.lock

  let find_opt t key =
    Lock.acquire t.lock;
    let result = Hashtbl.find_opt t.htbl key in
    Lock.release t.lock;
    result

  let replace t key value =
    Lock.acquire t.lock;
    Hashtbl.replace t.htbl key value;
    Lock.release t.lock

  let remove t key =
    Lock.acquire t.lock;
    Hashtbl.remove t.htbl key;
    Lock.release t.lock
end

let run_one ~budgetf ~n_domains ?(n_ops = 100 * Util.iter_factor)
    ?(n_keys = 1000) ~percent_mem ?(percent_add = (100 - percent_mem + 1) / 2)
    ?(prepopulate = true) ~lock_type () =
  let limit_mem = percent_mem in
  let limit_add = percent_mem + percent_add in

  assert (0 <= limit_mem && limit_mem <= 100);
  assert (limit_mem <= limit_add && limit_add <= 100);

  let t = Hashtbl_lock.create () in

  let n_ops = (100 + percent_mem) * n_ops / 100 in
  let n_ops = n_ops * n_domains in

  let n_ops_todo = Countdown.create ~n_domains () in

  let before () =
    begin
      match lock_type with
      | `Lock ->
          Hashtbl_lock.clear t;
          if prepopulate then begin
            for _ = 1 to n_keys do
              let value = Random.bits () in
              let key = value mod n_keys in
              Hashtbl_lock.replace t key value
            done
          end
    end;
    Countdown.non_atomic_set n_ops_todo n_ops
  in
  let init _ = Scheduler.run @@ fun () -> Random.State.make_self_init () in
  let wrap _ _ = Scheduler.run in
  let work domain_index state =
    match lock_type with
    | `Lock ->
        let rec work () =
          let n = Countdown.alloc n_ops_todo ~domain_index ~batch:1000 in
          if n <> 0 then begin
            for _ = 1 to n do
              let value = Random.State.bits state in
              let op = (value asr 20) mod 100 in
              let key = value mod n_keys in
              if op < percent_mem then Hashtbl_lock.find_opt t key |> ignore
              else if op < limit_add then Hashtbl_lock.replace t key value
              else Hashtbl_lock.remove t key
            done;
            work ()
          end
        in
        work ()
  in

  let config =
    Printf.sprintf "%d worker%s, %d%% reads with %s" n_domains
      (if n_domains = 1 then "" else "s")
      percent_mem
      (match lock_type with `Lock -> "Lock")
  in
  Times.record ~budgetf ~n_domains ~n_warmups:1 ~n_runs_min:1 ~before ~init
    ~wrap ~work ()
  |> Times.to_thruput_metrics ~n:n_ops ~singular:"operation" ~config

let run_suite ~budgetf =
  Util.cross [ 1; 2; 4; 8 ] (Util.cross [ `Lock ] [ 10; 50; 90; 95; 100 ])
  |> List.concat_map @@ fun (n_domains, (lock_type, percent_mem)) ->
     if Picos_domain.recommended_domain_count () < n_domains then []
     else run_one ~budgetf ~n_domains ~percent_mem ~lock_type ()
