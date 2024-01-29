type ty =
  | TVar of string
  | TArrow of ty * ty
  | TTop
  | TBot

type term =
  | Var of string
  | Abs of string * ty * term
  | App of term * term
  | TAbs of string * ty * term
  | TApp of term * ty

let rec is_subtype t1 t2 =
  match (t1, t2) with
  | _, TTop -> true
  | TBot, _ -> true
  | TArrow (a1, b1), TArrow (a2, b2) ->
      (is_subtype a2 a1) && (is_subtype b1 b2)
  | _, _ -> false

let rec type_of ctx term =
  match term with
  | Var x ->
      (try List.assoc x ctx with Not_found -> failwith "Variable not found")
  | Abs (x, ty, t) -> TArrow (ty, type_of ((x, ty) :: ctx) t)
  | App (t1, t2) ->
      let ty1 = type_of ctx t1 in
      let ty2 = type_of ctx t2 in
      (match ty1 with
      | TArrow (a, b) when is_subtype ty2 a -> b
      | _ -> failwith "Type mismatch in application")
  | TAbs (x, ty, t) -> TArrow (ty, type_of ((x, ty) :: ctx) t)
  | TApp (t, ty) ->
      let ty' = type_of ctx t in
      if is_subtype ty' ty then ty else failwith "Type mismatch in type application"

let rec show_ty ty =
  match ty with
  | TVar s -> s
  | TArrow (t1, t2) -> "(" ^ show_ty t1 ^ " -> " ^ show_ty t2 ^ ")"
  | TTop -> "Top"
  | TBot -> "Bot"


let example_term = App (TAbs ("x", TTop, Var "x"), Abs ("y", TBot, Var "y"))

let () =
  let ty = type_of [] example_term in
  Printf.printf "Type of term: %s\n" (show_ty ty)


