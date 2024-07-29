open Picos

type 'a request = {
  request : 'r. (unit -> 'r) Computation.t -> ('a -> 'r) -> unit;
}
[@@unboxed]

type 'a t =
  | Request : 'a request -> 'a t
  | Choose : 'a t list -> 'a t
  | Wrap : { event : 'b t; fn : 'b -> 'a } -> 'a t

type ('a, 'r) id = Yes : ('a, 'a) id | No : ('a, 'r) id

let rec request_1_as :
    type a r. (_ -> r) Computation.t -> (a -> r) -> (a, r) id -> a t -> _ =
 fun target to_result id -> function
  | Request { request } -> request target to_result
  | Choose ts -> request_n_as target to_result id ts
  | Wrap { event; fn } ->
      let to_result =
        match id with No -> fun x -> to_result (fn x) | Yes -> fn
      in
      request_1_as target to_result No event

and request_n_as :
    type a r. (_ -> r) Computation.t -> (a -> r) -> (a, r) id -> a t list -> _ =
 fun target to_result id -> function
  | [] -> ()
  | t :: ts ->
      request_1_as target to_result id t;
      request_n_as target to_result id ts

type ('a, _) tycon = Id : ('a, 'a t) tycon | List : ('a, 'a t list) tycon

let sync_as : type a n. n -> (a, n) tycon -> a =
 fun t n ->
  let target = Computation.create ~mode:`LIFO () in
  match
    match n with
    | Id -> request_1_as target Fun.id Yes t
    | List -> request_n_as target Fun.id Yes t
  with
  | () ->
      if Computation.is_running target then begin
        let t = Trigger.create () in
        if Computation.try_attach target t then
          match Trigger.await t with
          | None -> ()
          | Some exn_bt ->
              if Computation.try_cancel target exn_bt then Exn_bt.raise exn_bt
      end;
      Computation.await target ()
  | exception exn ->
      let exn_bt = Exn_bt.get exn in
      Computation.cancel target exn_bt;
      Exn_bt.raise exn_bt

let guard create_event =
  let request target to_result =
    request_1_as target to_result No (create_event ())
  in
  Request { request }

let[@alert "-handler"] from_computation source =
  let request target to_result =
    let result () = to_result (Computation.await source) in
    if Computation.is_running source then begin
      let propagator =
        Trigger.from_action result target @@ fun _ result target ->
        Computation.return target result
      in
      if Computation.try_attach source propagator then begin
        let detacher =
          Trigger.from_action propagator source @@ fun _ propagator source ->
          Computation.detach source propagator
        in
        if not (Computation.try_attach target detacher) then
          Computation.detach source propagator
      end
      else Computation.return target result
    end
    else Computation.return target result
  in
  Request { request }

let always value =
  let request computation to_result =
    Computation.return computation @@ fun () -> to_result value
  in
  Request { request }

type 'a event = 'a t

let[@inline] from_request p = Request p
let[@inline] choose ts = Choose ts
let[@inline] wrap event fn = Wrap { event; fn }
let[@inline] map fn event = Wrap { event; fn }
let[@inline] sync t = sync_as t Id
let[@inline] select ts = sync_as ts List
