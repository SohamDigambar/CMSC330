open MicroCamlTypes
open Utils
open TokenTypes

(* Provided functions - DO NOT MODIFY *)

(* Matches the next token in the list, throwing an error if it doesn't match the given token *)
let match_token (toks : token list) (tok : token) =
  match toks with
  | [] -> raise (InvalidInputException (string_of_token tok))
  | h :: t when h = tok -> t
  | h :: _ ->
      raise
        (InvalidInputException
           (Printf.sprintf "Expected %s from input %s, got %s"
              (string_of_token tok)
              (string_of_list string_of_token toks)
              (string_of_token h)))

(* Matches a sequence of tokens given as the second list in the order in which they appear, throwing an error if they don't match *)
let match_many (toks : token list) (to_match : token list) =
  List.fold_left match_token toks to_match

(* Return the next token in the token list as an option *)
let lookahead (toks : token list) =
  match toks with [] -> None | h :: t -> Some h

(* Return the token at the nth index in the token list as an option*)
let rec lookahead_many (toks : token list) (n : int) =
  match (toks, n) with
  | h :: _, 0 -> Some h
  | _ :: t, n when n > 0 -> lookahead_many t (n - 1)
  | _ -> None

(* Part 2: Parsing expressions *)

let rec parse_expr toks =
  match lookahead toks with
  | Some Tok_Let -> parse_let toks
  | Some Tok_If -> parse_if toks
  | Some Tok_Fun -> parse_fun toks
  | _ -> parse_or toks

and parse_prim_expr toks =
  match lookahead toks with
  | Some (Tok_Int x) ->
      let curr_tok = match_token toks (Tok_Int x) in
      (curr_tok, Value (Int x))
  | Some (Tok_Bool x) ->
      let curr_tok = match_token toks (Tok_Bool x) in
      (curr_tok, Value (Bool x))
  | Some (Tok_String x) ->
      let curr_tok = match_token toks (Tok_String x) in
      (curr_tok, Value (String x))
  | Some (Tok_ID x) ->
      let curr_tok = match_token toks (Tok_ID x) in
      (curr_tok, ID x)
  | Some Tok_LParen ->
      let curr_tok = match_token toks Tok_LParen in
      let curr_tok1, expr = parse_expr curr_tok in
      let curr_tok2 = match_token curr_tok1 Tok_RParen in
      (curr_tok2, expr)
  | _ -> raise (InvalidInputException "parse primary expression error")

and parse_fun_call toks =
  let curr_toks, expr = parse_prim_expr toks in
  match lookahead curr_toks with
  | Some (Tok_Int x) ->
      let curr_tok1, expr1 = parse_prim_expr curr_toks in
      (curr_tok1, FunctionCall (expr, expr1))
  | Some (Tok_Bool x) ->
      let curr_tok1, expr1 = parse_prim_expr curr_toks in
      (curr_tok1, FunctionCall (expr, expr1))
  | Some (Tok_String x) ->
      let curr_tok1, expr1 = parse_prim_expr curr_toks in
      (curr_tok1, FunctionCall (expr, expr1))
  | Some (Tok_ID x) ->
      let curr_tok1, expr1 = parse_prim_expr curr_toks in
      (curr_tok1, FunctionCall (expr, expr1))
  | Some Tok_LParen ->
      let curr_tok1, expr1 = parse_prim_expr curr_toks in
      (curr_tok1, FunctionCall (expr, expr1))
  | _ -> parse_prim_expr toks

and parse_unary toks =
  match lookahead toks with
  | Some Tok_Not ->
      let curr_toks1 = match_token toks Tok_Not in
      let curr_toks2, expr = parse_unary curr_toks1 in
      (curr_toks2, Not expr)
  | _ -> parse_fun_call toks

and parse_concat toks =
  let curr_toks1, expr1 = parse_unary toks in
  match lookahead curr_toks1 with
  | Some Tok_Concat ->
      let curr_toks2 = match_token curr_toks1 Tok_Concat in
      let curr_toks3, expr2 = parse_concat curr_toks2 in
      (curr_toks3, Binop (Concat, expr1, expr2))
  | _ -> (curr_toks1, expr1)

and parse_mult toks =
  let curr_toks1, expr1 = parse_concat toks in
  match lookahead curr_toks1 with
  | Some Tok_Mult ->
      let curr_toks2 = match_token curr_toks1 Tok_Mult in
      let curr_toks3, expr2 = parse_mult curr_toks2 in
      (curr_toks3, Binop (Mult, expr1, expr2))
  | Some Tok_Div ->
      let curr_toks2 = match_token curr_toks1 Tok_Div in
      let curr_toks3, expr2 = parse_mult curr_toks2 in
      (curr_toks3, Binop (Div, expr1, expr2))
  | _ -> parse_concat toks

and parse_add toks =
  let curr_toks1, expr1 = parse_mult toks in
  match lookahead curr_toks1 with
  | Some Tok_Add ->
      let curr_toks2 = match_token curr_toks1 Tok_Add in
      let curr_toks3, expr2 = parse_add curr_toks2 in
      (curr_toks3, Binop (Add, expr1, expr2))
  | Some Tok_Sub ->
      let curr_toks2 = match_token curr_toks1 Tok_Sub in
      let curr_toks3, expr2 = parse_add curr_toks2 in
      (curr_toks3, Binop (Sub, expr1, expr2))
  | _ -> parse_mult toks

and parse_rel_expr toks =
  let curr_toks1, expr1 = parse_add toks in
  match lookahead curr_toks1 with
  | Some Tok_Less ->
      let curr_toks2 = match_token curr_toks1 Tok_Less in
      let curr_toks3, expr2 = parse_rel_expr curr_toks2 in
      (curr_toks3, Binop (Less, expr1, expr2))
  | Some Tok_Greater ->
      let curr_toks2 = match_token curr_toks1 Tok_Greater in
      let curr_toks3, expr2 = parse_rel_expr curr_toks2 in
      (curr_toks3, Binop (Greater, expr1, expr2))
  | Some Tok_LessEqual ->
      let curr_toks2 = match_token curr_toks1 Tok_LessEqual in
      let curr_toks3, expr2 = parse_rel_expr curr_toks2 in
      (curr_toks3, Binop (LessEqual, expr1, expr2))
  | Some Tok_GreaterEqual ->
      let curr_toks2 = match_token curr_toks1 Tok_GreaterEqual in
      let curr_toks3, expr2 = parse_rel_expr curr_toks2 in
      (curr_toks3, Binop (GreaterEqual, expr1, expr2))
  | _ -> (curr_toks1, expr1)

and parse_equality toks =
  let curr_toks1, expr1 = parse_rel_expr toks in
  match lookahead curr_toks1 with
  | Some Tok_Equal ->
      let curr_toks2 = match_token curr_toks1 Tok_Equal in
      let curr_toks3, expr2 = parse_equality curr_toks2 in
      (curr_toks3, Binop (Equal, expr1, expr2))
  | Some Tok_NotEqual ->
      let curr_toks2 = match_token curr_toks1 Tok_NotEqual in
      let curr_toks3, expr2 = parse_equality curr_toks2 in
      (curr_toks3, Binop (NotEqual, expr1, expr2))
  | _ -> (curr_toks1, expr1)

and parse_and toks =
  let curr_toks1, expr1 = parse_equality toks in
  match lookahead curr_toks1 with
  | Some Tok_And ->
      let curr_toks2 = match_token curr_toks1 Tok_And in
      let curr_toks3, expr2 = parse_and curr_toks2 in
      (curr_toks3, Binop (And, expr1, expr2))
  | _ -> (curr_toks1, expr1)

and parse_or toks =
  let curr_toks1, expr1 = parse_and toks in
  match lookahead curr_toks1 with
  | Some Tok_Or ->
      let curr_toks2 = match_token curr_toks1 Tok_Or in
      let curr_toks3, expr2 = parse_or curr_toks2 in
      (curr_toks3, Binop (Or, expr1, expr2))
  | _ -> (curr_toks1, expr1)

and parse_if toks =
  let curr_toks = match_token toks Tok_If in
  let curr_toks2, expr1 = parse_expr curr_toks in
  let curr_toks3 = match_token curr_toks2 Tok_Then in
  let curr_toks4, expr2 = parse_expr curr_toks3 in
  let curr_toks5 = match_token curr_toks4 Tok_Else in
  let curr_toks6, expr3 = parse_expr curr_toks5 in
  (curr_toks6, If (expr1, expr2, expr3))

and parse_fun toks =
  let curr_toks = match_token toks Tok_Fun in
  match lookahead curr_toks with
  | Some (Tok_ID id) ->
      let curr_toks1 = match_many curr_toks [ Tok_ID id; Tok_Arrow ] in
      let curr_tok2, expr = parse_expr curr_toks1 in
      (curr_tok2, Fun (id, expr))
  | _ -> raise (InvalidInputException "parse if expression error")

and parse_let toks =
  let toks_no_let = match_token toks Tok_Let in
  (* returns list WITHOUT most recent lookahead (Let) *)
  let check_rec = lookahead toks_no_let = Some Tok_Rec in
  let curr_toks =
    if check_rec then match_token toks_no_let Tok_Rec else toks_no_let
  in
  match lookahead curr_toks with
  | Some (Tok_ID id) ->
      let curr_toks1 = match_many curr_toks [ Tok_ID id; Tok_Equal ] in
      let curr_toks2, expr1 = parse_expr curr_toks1 in
      let curr_toks3 = match_token curr_toks2 Tok_In in
      let curr_toks4, expr2 = parse_expr curr_toks3 in
      if List.mem Tok_DoubleSemi curr_toks4 then
        let curr_toks5 = match_token curr_toks4 Tok_DoubleSemi in
        (curr_toks5, Let (id, check_rec, expr1, expr2))
      else (curr_toks4, Let (id, check_rec, expr1, expr2))
  | _ -> raise (InvalidInputException "parse let expression error")
(* Part 3: Parsing mutop *)

let rec parse_mutop toks =
  match lookahead toks with
  | Some Tok_Def -> (
      let curr_toks = match_token toks Tok_Def in
      match lookahead curr_toks with
      | Some (Tok_ID id) ->
          let curr_toks1 = match_many curr_toks [ Tok_ID id; Tok_Equal ] in
          let curr_toks2, expr = parse_expr curr_toks1 in
          let curr_toks3 = match_token curr_toks2 Tok_DoubleSemi in
          (curr_toks3, Def (id, expr))
      | _ -> raise (InvalidInputException "parse mutop expression error"))
  | Some Tok_DoubleSemi -> (match_token toks Tok_DoubleSemi, NoOp)
  | _ ->
      let curr_toks, expr = parse_expr toks in
      (match_token curr_toks Tok_DoubleSemi, Expr expr)
