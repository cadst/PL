module F = Format

open Ast
open Value

let rec interp_expr (e: expr) (g: FStore.t) (s: Store.t) : Value.t =
  match e with
  | Num n -> NumV n
  | Add (e1, e2) ->
      (match interp_expr e1 g s, interp_expr e2 g s with
      | NumV n1, NumV n2 -> NumV (n1 + n2))
  | Sub (e1, e2) ->
      (match interp_expr e1 g s, interp_expr e2 g s with
      | NumV n1, NumV n2 -> NumV (n1 - n2))
  | Id x ->
      (try Store.find x s with Not_found -> failwith ("Free identifier: " ^ x))
  | LetIn (x, e1, e2) ->
      let v = interp_expr e1 g s in
      let s' = Store.add x v s in
      interp_expr e2 g s'
  | Call (fname, args) ->
      let (params, body) =
        (try FStore.find fname g with Not_found -> failwith ("Undefined function: " ^ fname))
      in
      if List.length params <> List.length args then
        failwith ("The number of arguments of " ^ fname ^ " mismatched: Required: " ^ string_of_int (List.length params) ^ ", Actual: " ^ string_of_int (List.length args))
      else
        let rec bind_params params args env =
          match params, args with
          | [], [] -> env
          | p::ps, a::as_ -> bind_params ps as_ (Store.add p (interp_expr a g env) env)
          | _ -> failwith ("The number of arguments of " ^ fname ^ " mismatched: Required: " ^ string_of_int (List.length params) ^ ", Actual: " ^ string_of_int (List.length args))
        in
        let new_env = bind_params params args s in
        interp_expr body g new_env

let interp_fundef (d: fundef) (g: FStore.t) : FStore.t =
  match d with
  | FunDef (name, params, body) -> FStore.add name (params, body) g


let interp (p: prog) : Value.t =
  match p with
  | Prog (fundefs, expr) ->
      let global_env = List.fold_left (fun g f -> interp_fundef f g) FStore.empty fundefs in
      interp_expr expr global_env Store.empty

