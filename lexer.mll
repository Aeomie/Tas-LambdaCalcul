{ open Parser
  exception Eof
 }
rule token = parse
  | [' ' '\t' '\r' '\n']       { token lexbuf }  (* Skip whitespace *)
  | "("                        { LPAREN }
  | ")"                        { RPAREN }
  | "+"                        { PLUS }
  | "-"                        { MINUS }
  | "->"                       { ARROW }
  | "fun"                      { FUN }
  | ['a'-'z' 'A'-'Z' '_']['a'-'z' 'A'-'Z' '0'-'9' '_']* as id { IDENT id }
  | ['0'-'9']+ as num           { INT (int_of_string num) }
  | eof                         { EOF }
  | _                           { failwith "Unexpected character" }
