open Format

type term =
  | TmVar of int * int
  | TmAbs of string * term
  | TmApp of term * term

type binding = NameBind
type context = (string * binding) list

exception NoRuleApplies

let rec printtm ctx t = match t with 
  TmAbs( x,t1) -> let (ctx', x') = pick_fresh_name ctx x in 
  pr "(lambda "; pr x'; pr ". "; printtm ctx' t1; pr" )"
  | TmApp( t1, t2) -> pr "("; printtm ctx t1; pr" "; printtm ctx t2; pr ")"
  | TmVar( x,n) -> 
    if ctxlength ctx = n then
      pr (index2name fi ctx x)
    else
      pr "[bad index]"

let termShift d t = let rec walk c t = match t with 
      TmVar( x,n) -> if x>=c then TmVar( x+d, n+d) else TmVar( x,n+d)|
      TmAbs( x, t1) -> TmAbs( x, walk (c+1) t1)|
      TmApp( t1, t2) -> TmApp( walk c t1, walk c t2) in walk 0 t 

let termSubst j s t = let rec walk c t = match t with 
      TmVar( x, n) -> if x=j+c then termShift c s else TmVar( x,n)
      |TmAbs( x, t1) -> TmAbs( x, walk (c+1) t1)
      |TmApp( t1,t2) -> TmApp( walk c t1, walk c t2)
    in walk 0 t 

let termSubstTop s t = termShift (-1) (termSubst 0 (termShift 1 s) t)

let rec isval ctx t = match t with 
    TmAbs(_,_,_) -> true |
    _ -> false 

let rec eval1 ctx t = match t with 
    TmApp(TmAbs(x,t12),v2) when isval ctx v2 -> termSubstTop v2 v12 |
    TmApp(v1,t2) when isval ctx v1 -> let t22 = eval1 ctx t2 in TmApp(v1, t22)|
    TmApp(t1,t2) -> let t11 = eval1 ctx t1 in TmApp(t11, t2)|
    _ -> raise NoRuleApplies

let rec eval ctx t = 
  try let t1 = eval1 ctx t in eval ctx t1 with NoRuleApplies -> t 