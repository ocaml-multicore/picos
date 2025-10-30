let has_domains = 1 < Domain.recommended_domain_count ()
let n = 128 * 4
let counters = Array.init n (fun _ -> Atomic.make 0)

let[@inline never] once_at counter ~log_scale =
  if has_domains then begin
    let n_contending_threads = Atomic.fetch_and_add counter 1 + 1 in
    let n = ref (Random.int ((n_contending_threads lsl log_scale) + 0)) in
    while 0 <= !n do
      Domain.cpu_relax ();
      decr n
    done;
    Atomic.decr counter
  end

let[@inline never] once ~random_key ~log_scale =
  let i = random_key land (n - 1) in
  let counter = Array.unsafe_get counters i in
  once_at counter ~log_scale

let[@inline] once_unless_alone ~random_key ~log_scale =
  let i = random_key land (n - 1) in
  let counter = Array.unsafe_get counters i in
  if 0 <> Atomic.get counter then once_at counter ~log_scale
