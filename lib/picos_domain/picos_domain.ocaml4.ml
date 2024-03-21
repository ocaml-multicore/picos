let at_exit = Stdlib.at_exit
let recommended_domain_count () = 1

module DLS = struct
  type 'a state = Value of { mutable value : 'a } | Default of (unit -> 'a)
  type 'a key = 'a state ref

  let new_key default = ref (Default default)

  let[@poll error] [@inline never] compare_and_set key before after =
    !key == before
    && begin
         key := after;
         true
       end

  let rec get key =
    match !key with
    | Value r -> r.value
    | Default default as before ->
        let value = default () in
        if compare_and_set key before (Value { value }) then value else get key

  let rec set key value =
    match !key with
    | Value r -> r.value <- value
    | Default _ as before ->
        if not (compare_and_set key before (Value { value })) then set key value
end
