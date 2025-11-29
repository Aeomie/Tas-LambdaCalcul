type token =
  | INT of (int)
  | IDENT of (string)
  | PLUS
  | MINUS
  | COMMA
  | LPAREN
  | RPAREN
  | LBRACKET
  | RBRACKET
  | ARROW
  | FUN
  | LET
  | IN
  | EQUAL
  | FIX
  | HD
  | TL
  | CONS
  | IFZERO
  | IFEMPTY
  | THEN
  | ELSE
  | DEREF
  | REF
  | ASSIGN
  | UNIT
  | EOF

val prog :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.term
