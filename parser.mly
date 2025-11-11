%{
open Ast
%}

%token <int> INT
%token <string> IDENT
%token PLUS LPAREN RPAREN ARROW FUN EOF

%type <Ast.term> prog
%start prog

%%
prog:
  expr EOF { $1 }
;
expr:
    INT                 { N($1) }
  | IDENT               { Var($1) }
  | expr PLUS expr      { Add($1, $3) }
  | expr expr           { App($1, $2) }
  | FUN IDENT ARROW expr { Abs($2, $4) }
  | LPAREN expr RPAREN  { $2 }
;