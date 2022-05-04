open TokenTypes
open String
open Str

(* PASTE YOUR LEXER FROM P4A HERE *)

let tokenize input = 
  let rec tok pos =
  if pos >= length input then []
  else if string_match (regexp "[ \t\n]") input pos then
    tok (pos + length (matched_string input))
  else if string_match (regexp {|[0-9]+|}) input pos then
    let token = matched_string input in
    Tok_Int (int_of_string token) :: tok (pos + length token)
  else if string_match (regexp {|(-[0-9]+)|}) input pos then
    let token = matched_string input in
    Tok_Int
      (int_of_string
         (string_after (string_before token (length token - 1)) 1))
    :: tok (pos + length token)
  else if string_match (regexp {|\"[^\"]*\"|}) input pos then
    let token = matched_string input in
    Tok_String (string_after (string_before token (length token - 1)) 1)
    :: tok (pos + length token)
  else if string_match (regexp {|true\|false|}) input pos then
    let token = matched_string input in
    Tok_Bool (bool_of_string token) :: tok (pos + length token)
  else if string_match (regexp {|not|}) input pos then
    Tok_Not :: tok (pos + length (matched_string input))
  else if string_match (regexp {|if|}) input pos then
    Tok_If :: tok (pos + length (matched_string input))
  else if string_match (regexp {|then|}) input pos then
    Tok_Then :: tok (pos + length (matched_string input))
  else if string_match (regexp {|else|}) input pos then
    Tok_Else :: tok (pos + length (matched_string input))
  else if string_match (regexp {|let|}) input pos then
    Tok_Let :: tok (pos + length (matched_string input))
  else if string_match (regexp {|def|}) input pos then
    Tok_Def :: tok (pos + length (matched_string input))
  else if string_match (regexp {|in|}) input pos then
    Tok_In :: tok (pos + length (matched_string input))
  else if string_match (regexp {|rec|}) input pos then
    Tok_Rec :: tok (pos + length (matched_string input))
  else if string_match (regexp {|fun|}) input pos then
    Tok_Fun :: tok (pos + length (matched_string input))
  else if string_match (regexp {|[a-zA-Z][a-zA-Z0-9]*|}) input pos then
    let token = matched_string input in
    Tok_ID token :: tok (pos + length token)
  else if string_match (regexp {|;;|}) input pos then
    Tok_DoubleSemi :: tok (pos + length (matched_string input))
  else if string_match (regexp {|->|}) input pos then
    Tok_Arrow :: tok (pos + length (matched_string input))
  else if string_match (regexp "||") input pos then
    Tok_Or :: tok (pos + length (matched_string input))
  else if string_match (regexp "&&") input pos then
    Tok_And :: tok (pos + length (matched_string input))
  else if string_match (regexp {|+|}) input pos then
    Tok_Add :: tok (pos + length (matched_string input))
  else if string_match (regexp {|-|}) input pos then
    Tok_Sub :: tok (pos + length (matched_string input))
  else if string_match (regexp {|/|}) input pos then
    Tok_Div :: tok (pos + length (matched_string input))
  else if string_match (regexp {|*|}) input pos then
    Tok_Mult :: tok (pos + length (matched_string input))
  else if string_match (regexp "\\^") input pos then
    Tok_Concat :: tok (pos + length (matched_string input))
  else if string_match (regexp "(") input pos then
    Tok_LParen :: tok (pos + length (matched_string input))
  else if string_match (regexp ")") input pos then
    Tok_RParen :: tok (pos + length (matched_string input))
  else if string_match (regexp {|=|}) input pos then
    Tok_Equal :: tok (pos + length (matched_string input))
  else if string_match (regexp {|<>|}) input pos then
    Tok_NotEqual :: tok (pos + length (matched_string input))
  else if string_match (regexp {|>=|}) input pos then
    Tok_GreaterEqual :: tok (pos + length (matched_string input))
  else if string_match (regexp {|<=|}) input pos then
    Tok_LessEqual :: tok (pos + length (matched_string input))
  else if string_match (regexp {|>|}) input pos then
    Tok_Greater :: tok (pos + length (matched_string input))
  else if string_match (regexp {|<|}) input pos then
    Tok_Less :: tok (pos + length (matched_string input))
  else raise (InvalidInputException "invalid input to tokenize")
  (* if there is an unrecognizable regexp, then throw exception *)
in
tok 0