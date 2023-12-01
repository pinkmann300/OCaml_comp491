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
  | _ -> false

(** Doubt for prof - Why have they excluded the PredCase in the isnumerical implementation in the textbook? Counter example 
    I thought of : Pred(Succ t1) will not be recognized as a numerical value in that case. 
    
    Doubt clarified: 
    
    Pred will never be a term because it is merely a constructor. Succ and zero essentially cover all discrete numbers. 
    **)

let rec isbool t = match t with 
    T -> true|
    F -> true|
    _ -> false 

let rec isval t = match t with 
  T -> true|
  F -> true| 
  t when (isnumerical t) -> true |
  _ -> false


exception NoRuleApplies 

let rec eval1 t = match t with
  TmIf(T,t2,t3) -> (eval1 t2)|
  TmIf(F,t2,t3) -> (eval1 t3)|
  TmIf(t1,t2,t3) -> (eval1 (TmIf((eval1 t1), t2, t3)))|
  TmSucc(t1) -> TmSucc(eval1 t1)|
  TmPred(TmZero) -> TmZero|
  TmPred(TmSucc(t1)) when (isnumerical t1) -> t1|
  TmPred(t1) -> TmPred(eval1 t1)|
  TmIsZero(TmZero) -> T|
  TmIsZero(TmSucc(t1)) when (isnumerical t1) -> F|
  TmIsZero(t1) -> TmIsZero(eval1(t1))|
  _ -> raise NoRuleApplies


let () = 
  let result = factorial 5 in print_int result;
  print_newline ();; 
  let x1 = T 
  let x2 = F 
  let x5 = TmSucc(TmSucc(TmZero))

  let x7 = TmIf(TmZero,T,F);;
  let re1 = isval x7 in
  Printf.printf "TmIf(TmZero,T,F) is %b\n" re1; 

(** Untyped arithmetic expressions - Chapter 3 from Types and Programming Languages **)
