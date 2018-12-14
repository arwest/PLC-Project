// Signature file for parser generated by fsyacc
module PlcParser
type token = 
  | EOF
  | HASH
  | COMMA
  | COLON
  | SEMIC
  | LPAR
  | RPAR
  | LBRACE
  | RBRACE
  | LBRACK
  | RBRACK
  | EQ
  | NEQ
  | LT
  | LTE
  | PLUS
  | MINUS
  | TIMES
  | DIV
  | PRINT
  | CONS
  | HEAD
  | TAIL
  | ISE
  | NOT
  | AND
  | IF
  | THEN
  | ELSE
  | VAR
  | FUN
  | REC
  | DARROW
  | FN
  | END
  | UNIT
  | BOOL
  | INT
  | LIST
  | TUPLE
  | ARROW
  | CSTBOOL of (bool)
  | NAME of (string)
  | CSTINT of (int)
type tokenId = 
    | TOKEN_EOF
    | TOKEN_HASH
    | TOKEN_COMMA
    | TOKEN_COLON
    | TOKEN_SEMIC
    | TOKEN_LPAR
    | TOKEN_RPAR
    | TOKEN_LBRACE
    | TOKEN_RBRACE
    | TOKEN_LBRACK
    | TOKEN_RBRACK
    | TOKEN_EQ
    | TOKEN_NEQ
    | TOKEN_LT
    | TOKEN_LTE
    | TOKEN_PLUS
    | TOKEN_MINUS
    | TOKEN_TIMES
    | TOKEN_DIV
    | TOKEN_PRINT
    | TOKEN_CONS
    | TOKEN_HEAD
    | TOKEN_TAIL
    | TOKEN_ISE
    | TOKEN_NOT
    | TOKEN_AND
    | TOKEN_IF
    | TOKEN_THEN
    | TOKEN_ELSE
    | TOKEN_VAR
    | TOKEN_FUN
    | TOKEN_REC
    | TOKEN_DARROW
    | TOKEN_FN
    | TOKEN_END
    | TOKEN_UNIT
    | TOKEN_BOOL
    | TOKEN_INT
    | TOKEN_LIST
    | TOKEN_TUPLE
    | TOKEN_ARROW
    | TOKEN_CSTBOOL
    | TOKEN_NAME
    | TOKEN_CSTINT
    | TOKEN_end_of_input
    | TOKEN_error
type nonTerminalId = 
    | NONTERM__startMain
    | NONTERM_Main
    | NONTERM_Prog
    | NONTERM_Expr
    | NONTERM_LExpr
    | NONTERM_AtExpr
    | NONTERM_AppExpr
    | NONTERM_Const
    | NONTERM_ListComps
    | NONTERM_Comps
    | NONTERM_HighArgs
    | NONTERM_Params
    | NONTERM_TypedVar
    | NONTERM_Type
    | NONTERM_AtType
    | NONTERM_Types
/// This function maps tokens to integer indexes
val tagOfToken: token -> int

/// This function maps integer indexes to symbolic token ids
val tokenTagToTokenId: int -> tokenId

/// This function maps production indexes returned in syntax errors to strings representing the non terminal that would be produced by that production
val prodIdxToNonTerminal: int -> nonTerminalId

/// This function gets the name of a token as a string
val token_to_string: token -> string
val Main : (Microsoft.FSharp.Text.Lexing.LexBuffer<'cty> -> token) -> Microsoft.FSharp.Text.Lexing.LexBuffer<'cty> -> (Absyn.expr) 
