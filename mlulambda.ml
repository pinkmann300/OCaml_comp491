(** The following is an ML implementation of the Lambda Calculus which closely follows the approach taken in Types and Programming Languages by Benjamin Pierce. **)

type term =
  | Var of int
  | Abs of term
  | App of term * term

(* Shifting the indices of free variables when entering a subterm *)
let rec shift d c = function
  | Var k when k < c -> Var k
  | Var k -> Var (k + d)
  | Abs t -> Abs (shift d (c + 1) t)
  | App(t1, t2) -> App (shift d c t1, shift d c t2)

(* Substituting the variable with the given term *)
let rec subst j s = function
  | Var k when k = j -> s
  | Var k -> Var k
  | Abs t -> Abs (subst (j + 1) (shift 1 0 s) t)
  | App(t1, t2) -> App (subst j s t1, subst j s t2)

(* Beta-reduction step *)
let rec beta_reduce = function
  | App(Abs t1, t2) -> shift (-1) 0 (subst 0 (shift 1 0 t2) t1)
  | Var _ | Abs _ as t -> t
  | App(t1, t2) -> App (beta_reduce t1, beta_reduce t2)

(* Examples *)
let example1 =
  let term = App(Abs(Var 0), Var 1) in
  beta_reduce term

let example2 =
  let term = App(Abs(App(Var 0, Var 1)), Var 2) in
  beta_reduce term

module Printlambda = struct
  let rec string_of_term = function
    | Var k -> string_of_int k
    | Abs t -> "λ." ^ string_of_term t
    | App (t1, t2) -> "(" ^ string_of_term t1 ^ " " ^ string_of_term t2 ^ ")"
end


let () =
  let example1 =
    let term = App(Abs(Var 0), Var 1) in
    Printf.printf "Original: %s\n" (Printlambda.string_of_term term);
    Printf.printf "Reduced: %s\n" (Printlambda.string_of_term (beta_reduce term));
  in

  let example2 =
    let term = App(Abs(App(Var 0, Var 1)), Var 2) in
    Printf.printf "Original: %s\n" (Printlambda.string_of_term term);
    Printf.printf "Reduced: %s\n" (Printlambda.string_of_term (beta_reduce term));
  in

  example1;
  example2;
  ()
