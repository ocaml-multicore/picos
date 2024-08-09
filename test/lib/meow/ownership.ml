open Picos

exception Resource_leaked
exception Not_owner
exception Parent_is_dead

type resource =
  | Resource : {
      finally : 'a -> unit;
      value : 'a;
      mutable owner : Fiber.Maybe.t;
    }
      -> resource

type t = resource Dllist.node

let finalize (Resource r) = r.finally r.value

let owned_key =
  let finalize resources =
    if not (Dllist.is_empty resources) then
      let fiber = Fiber.current () in
      if Fiber.is_canceled fiber then Dllist.iter_l finalize resources
      else raise Resource_leaked
  in
  Fiber.FLS.new_key ~finalize (Computed Dllist.create)

let own_as fiber resource =
  let (Resource r) = Dllist.value resource in
  if r.owner != Fiber.Maybe.nothing then
    invalid_arg "Resource already owned by some fiber";
  r.owner <- Fiber.Maybe.of_fiber fiber;
  let owned = Fiber.FLS.get fiber owned_key in
  Dllist.move_l owned resource

let own resource = own_as (Fiber.current ()) resource

let create ~finally value =
  Dllist.new_node (Resource { finally; value; owner = Fiber.Maybe.nothing })

type _ tdt =
  | Finalized : [> `Finalized ] tdt
  | Nil : [> `Nil ] tdt
  | Blessed : {
      resource : t;
      next : [ `Nil | `Blessed ] tdt;
    }
      -> [> `Blessed ] tdt

let rec iter action = function
  | Nil -> ()
  | Blessed r ->
      action r.resource;
      iter action r.next

let blessed_key : [ `Finalized | `Nil | `Blessed ] tdt Atomic.t Fiber.FLS.key =
  let finalize t =
    match Atomic.exchange t Finalized with
    | Finalized -> ()
    | (Nil as resources) | (Blessed _ as resources) ->
        resources |> iter @@ fun node -> finalize (Dllist.value node)
  in
  Fiber.FLS.new_key ~finalize (Computed (fun () -> Atomic.make Nil))

let[@inline never] accept_as fiber blessed =
  match Atomic.exchange blessed Nil with
  | Finalized -> failwith "accept after finalize"
  | (Nil as resources) | (Blessed _ as resources) ->
      let owned = Fiber.FLS.get fiber owned_key in
      resources
      |> iter @@ fun node ->
         let (Resource r) = Dllist.value node in
         r.owner <- Fiber.Maybe.of_fiber fiber;
         Dllist.move_l owned node

let[@inline] accept_as fiber =
  let blessed = Fiber.FLS.get fiber blessed_key in
  if Atomic.get blessed != Nil then accept_as fiber blessed

let check_as fiber resource =
  accept_as fiber;
  let (Resource r) = Dllist.value resource in
  if Fiber.Maybe.unequal r.owner (Fiber.Maybe.of_fiber fiber) then
    raise Not_owner

let check resource = check_as (Fiber.current ()) resource

let disown_as fiber resource =
  check_as fiber resource;
  Dllist.remove resource;
  let (Resource r) = Dllist.value resource in
  r.owner <- Fiber.Maybe.nothing

let disown resource = disown_as (Fiber.current ()) resource

let parent_key =
  let initialize parent = Fiber.FLS.get parent blessed_key
  and root () = invalid_arg "Root fiber has no parent" in
  Fiber.FLS.new_key ~initialize (Computed root)

let rec bless_as fiber resource =
  let parent = Fiber.FLS.get fiber parent_key in
  match Atomic.get parent with
  | Finalized ->
      own_as fiber resource;
      raise Parent_is_dead
  | (Nil as before) | (Blessed _ as before) ->
      let after = Blessed { resource; next = before } in
      if not (Atomic.compare_and_set parent before after) then
        bless_as fiber resource

let bless resource =
  let fiber = Fiber.current () in
  disown_as fiber resource;
  bless_as fiber resource
