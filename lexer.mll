{ open Parser
  exception Eof
 }
rule token = parse
  | [' ' '\t' '\r' '\n']       { token lexbuf }  (* Skip whitespace *)
  | "("                        { LPAREN }
  | ")"                        { RPAREN }
  | "["                       { LBRACKET }
  | "]"                       { RBRACKET }
  | ","                        { COMMA }
  | "+"                        { PLUS }
  | "-"                        { MINUS }
  | "="                        { EQUAL }
  | "->"                       { ARROW }
  | "fun"                      { FUN }
  | "let"                      { LET }
  | "in"                       { IN }
  | "fix"                     { FIX }
  | "cons"                      { CONS }
  | "hd"                     { HD }
  | "tl"                     { TL }
  | "ifzero"                  { IFZERO }
  | "ifempty"                 { IFEMPTY }
  | "then"                     { THEN }
  | "else"                     { ELSE }
  | ['a'-'z' 'A'-'Z' '_']['a'-'z' 'A'-'Z' '0'-'9' '_']* as id { IDENT id }
  | ['0'-'9']+ as num           { INT (int_of_string num) }
  | eof                         { EOF }
  | _                           { failwith "Unexpected character" }
