type term = T
            | F
            | TmZero
            | TmSucc of term 
            | TmPred of term
            | TmIsZero of term
            | TmIf of term * term * term

(** These denote comments in OCaml. Very similar to the structure in Isabelle. **)

let rec factorial t = match t with
    0 -> 1
    | 1 -> 1
    | _ ->  (t * factorial(t - 1))

let rec isnumerical t = match t with 
    TmZero -> true
  | TmSucc(t1) -> isnumerical t1 
  | TmPred(t1) -> isnumerical t1
  | _ -> false


(** Doubt for prof - Why have they excluded the PredCase in the isnumerical implementation in the textbook? Counter example 
    I thought of : Pred(Succ t1) will not be recognized as a numerical value in that case. **)


let rec isval t = match t with 
  T -> true|
  F -> true| 
  t when (isnumerical t) -> true |
  _ -> false

let () = 
  let result = factorial 5 in print_int result;
  print_newline ();; 
  let x1 = T 
  let x2 = F 
  let x5 = TmSucc(TmSucc(TmZero))

  let x7 = TmIf(TmZero,T,F);;
  let re1 = isval x7 in print_int re1;
  print_newline ();; 