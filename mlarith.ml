let i = 10 
let x = 13.2 

(** These denote comments in OCaml. Very similar to the structure in Isabelle. **)

let rec factorial t = match t with
    0 -> 1
    | 1 -> 1
    | _ ->  (t * factorial(t - 1))

let () = 
  let result = factorial 5 in print_int result;
  print_newline ();; 
