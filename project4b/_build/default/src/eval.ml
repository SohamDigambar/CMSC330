open MicroCamlTypes
open Utils

exception TypeError of string
exception DeclareError of string
exception DivByZeroError

(* Provided functions - DO NOT MODIFY *)

(* Adds mapping [x:v] to environment [env] *)
let extend env x v = (x, ref v) :: env

(* Returns [v] if [x:v] is a mapping in [env]; uses the
   most recent if multiple mappings for [x] are present *)
let rec lookup env x =
  match env with
  | [] -> raise (DeclareError ("Unbound variable " ^ x))
  | (var, value) :: t -> if x = var then !value else lookup t x

(* Creates a placeholder mapping for [x] in [env]; needed
   for handling recursive definitions *)
let extend_tmp env x = (x, ref (Int 0)) :: env

(* Updates the (most recent) mapping in [env] for [x] to [v] *)
let rec update env x v =
  match env with
  | [] -> raise (DeclareError ("Unbound variable " ^ x))
  | (var, value) :: t -> if x = var then value := v else update t x v

(* Part 1: Evaluating expressions *)

(* Evaluates MicroCaml expression [e] in environment [env],
   returning a value, or throwing an exception on error *)
let rec eval_expr env e =
  match e with
  | Value value -> value
  | ID id ->
      if List.length env > 0 then lookup env id
      else raise (DeclareError ("Unbound variable " ^ id))
  | Not expr ->
      let value = eval_expr env expr in
      if value = Bool true then Bool false
      else if value = Bool false then Bool true
      else raise (TypeError "Expected type bool")
  | Binop (op, expr1, expr2) -> (
      match op with
      | Add -> (
          match (eval_expr env expr1, eval_expr env expr2) with
          | Int x, Int y -> Int (x + y)
          | _ -> raise (TypeError "Expected type int"))
      | Sub -> (
          match (eval_expr env expr1, eval_expr env expr2) with
          | Int x, Int y -> Int (x - y)
          | _ -> raise (TypeError "Expected type int"))
      | Mult -> (
          match (eval_expr env expr1, eval_expr env expr2) with
          | Int x, Int y -> Int (x * y)
          | _ -> raise (TypeError "Expected type int"))
      | Div -> (
          match (eval_expr env expr1, eval_expr env expr2) with
          | Int x, Int y -> if y <> 0 then Int (x / y) else raise DivByZeroError
          | _ -> raise (TypeError "Expected type int"))
      | Concat -> (
          match (eval_expr env expr1, eval_expr env expr2) with
          | String str1, String str2 -> String (str1 ^ str2)
          | _ -> raise (TypeError "Expected type string"))
      | Greater -> (
          match (eval_expr env expr1, eval_expr env expr2) with
          | Int x, Int y -> Bool (x > y)
          | _ -> raise (TypeError "Expected type int"))
      | Less -> (
          match (eval_expr env expr1, eval_expr env expr2) with
          | Int x, Int y -> Bool (x < y)
          | _ -> raise (TypeError "Expected type int"))
      | GreaterEqual -> (
          match (eval_expr env expr1, eval_expr env expr2) with
          | Int x, Int y -> Bool (x >= y)
          | _ -> raise (TypeError "Expected type int"))
      | LessEqual -> (
          match (eval_expr env expr1, eval_expr env expr2) with
          | Int x, Int y -> Bool (x <= y)
          | _ -> raise (TypeError "Expected type int"))
      | Equal -> (
          match eval_expr env expr1 with
          | Int x -> (
              match eval_expr env expr2 with
              | Int y -> Bool (x = y)
              | _ -> raise (TypeError "Expected type int"))
          | String str1 -> (
              match eval_expr env expr2 with
              | String str2 -> Bool (str1 = str2)
              | _ -> raise (TypeError "Expected type string"))
          | Bool flag1 -> (
              match eval_expr env expr2 with
              | Bool flag2 -> Bool (flag1 = flag2)
              | _ -> raise (TypeError "Expected type bool"))
          | _ -> raise (TypeError "Cannot compare types"))
      | NotEqual -> (
          match eval_expr env expr1 with
          | Int x -> (
              match eval_expr env expr2 with
              | Int y -> Bool (x <> y)
              | _ -> raise (TypeError "Expected type int"))
          | String str1 -> (
              match eval_expr env expr2 with
              | String str2 -> Bool (str1 <> str2)
              | _ -> raise (TypeError "Expected type string"))
          | Bool flag1 -> (
              match eval_expr env expr2 with
              | Bool flag2 -> Bool (flag1 <> flag2)
              | _ -> raise (TypeError "Expected type bool"))
          | _ -> raise (TypeError "Cannot compare types"))
      | Or -> (
          match (eval_expr env expr1, eval_expr env expr2) with
          | Bool flag1, Bool flag2 -> Bool (flag1 || flag2)
          | _ -> raise (TypeError "Expected type bool"))
      | And -> (
          match (eval_expr env expr1, eval_expr env expr2) with
          | Bool flag1, Bool flag2 -> Bool (flag1 && flag2)
          | _ -> raise (TypeError "Expected type bool")))
  | If (expr1, expr2, expr3) ->
      let value1 = eval_expr env expr1 in
      if value1 = Bool true then eval_expr env expr2
      else if value1 = Bool false then eval_expr env expr3
      else raise (TypeError "Expected type bool")
  | Let (id, check_rec, expr1, expr2) ->
      if check_rec then
        let temp_env = extend_tmp env id in
        let value = eval_expr temp_env expr1 in
        let _ = update temp_env id value in
        eval_expr temp_env expr2
      else if check_rec = false then
        let value = eval_expr env expr1 in
        let new_env = extend env id value in
        eval_expr new_env expr2
      else raise (TypeError "Expected type Let")
  | Fun (fun_id, expr) -> Closure (env, fun_id, expr)
  | FunctionCall (expr1, expr2) -> (
      let value1 = eval_expr env expr1 in
      match value1 with
      | Closure (curr_env, id, e) ->
          let value2 = eval_expr env expr2 in
          let new_env = extend curr_env id value2 in
          eval_expr new_env e
      | _ -> raise (TypeError "Expected type FunctionCall"))

(* Part 2: Evaluating mutop directive *)

(* Evaluates MicroCaml mutop directive [m] in environment [env],
   returning a possibly updated environment paired with
   a value option; throws an exception on error *)
let eval_mutop env m =
  match m with
  | Def (var, expr) ->
      let temp_env = extend_tmp env var in
      let value = eval_expr temp_env expr in
      let _ = update temp_env var value in
      (temp_env, Some value)
  | Expr expr ->
      let value = eval_expr env expr in
      (env, Some value)
  | NoOp -> (env, None)
