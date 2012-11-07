// Signature file for parser generated by fsyacc
type token = 
  | EOF
  | VAR of (System.String)
  | NUM of (System.Int32)
  | EQ
  | ADD
  | MUL
  | NEG
  | POW
type tokenId = 
    | TOKEN_EOF
    | TOKEN_VAR
    | TOKEN_NUM
    | TOKEN_EQ
    | TOKEN_ADD
    | TOKEN_MUL
    | TOKEN_NEG
    | TOKEN_POW
    | TOKEN_end_of_input
    | TOKEN_error
type nonTerminalId = 
    | NONTERM__startstart
    | NONTERM_start
    | NONTERM_Equation
    | NONTERM_RightPart
    | NONTERM_LeftPart
    | NONTERM_Expr
/// This function maps integers indexes to symbolic token ids
val tagOfToken: token -> int

/// This function maps integers indexes to symbolic token ids
val tokenTagToTokenId: int -> tokenId

/// This function maps production indexes returned in syntax errors to strings representing the non terminal that would be produced by that production
val prodIdxToNonTerminal: int -> nonTerminalId

/// This function gets the name of a token as a string
val token_to_string: token -> string
val start : (Microsoft.FSharp.Text.Lexing.LexBuffer<'cty> -> token) -> Microsoft.FSharp.Text.Lexing.LexBuffer<'cty> -> ( Types.Equation ) 