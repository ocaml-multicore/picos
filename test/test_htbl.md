# `Picos_htbl`

```ocaml
# Random.self_init ()
- : unit = ()

# let t : (string, int) Picos_htbl.t = Picos_htbl.create ()
val t : (string, int) Picos_htbl.t = <abstr>

# Picos_htbl.try_add t "Basics" 101
- : bool = true

# Picos_htbl.try_add t "Answer" 42
- : bool = true

# Picos_htbl.remove_exn t "Basics"
- : int = 101

# Picos_htbl.try_remove t "Basics"
- : bool = false

# Picos_htbl.remove_all t |> List.of_seq
- : (string * int) list = [("Answer", 42)]

# Picos_htbl.to_seq t |> List.of_seq
- : (string * int) list = []

# ["One"; "Two"; "Three"]
  |> List.iteri @@ fun v k ->
     assert (Picos_htbl.try_add t k v)
- : unit = ()

# Picos_htbl.to_seq t
  |> List.of_seq
  |> List.sort (fun l r -> String.compare (fst l) (fst r))
- : (string * int) list = [("One", 0); ("Three", 2); ("Two", 1)]
```
