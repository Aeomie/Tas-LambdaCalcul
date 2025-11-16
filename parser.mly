%{
open Ast
%}

%token <int> INT
%token <string> IDENT
%token PLUS MINUS COMMA
%token LPAREN RPAREN LBRACKET RBRACKET
%token ARROW FUN LET IN EQUAL FIX
%token HD TL CONS IFZERO IFEMPTY THEN ELSE
%token EOF

%left PLUS MINUS
%nonassoc APP

%type <Ast.term> prog
%start prog

%%

prog:
  | expr EOF { $1 }
;

expr:
  | basic_expr                             { $1 }
  | expr expr %prec APP                       { App($1, $2) }
  | FUN IDENT ARROW expr                 { Abs($2, $4) }
  | LET IDENT EQUAL expr IN expr         { Let($2, $4, $6) }
  | FIX LPAREN IDENT ARROW expr RPAREN   { Fix($3, $5) }
  | IFZERO expr THEN expr ELSE expr      { IfZero($2, $4, $6) }
  | IFEMPTY expr THEN expr ELSE expr     { IfEmpty($2, $4, $6) }
  | expr PLUS expr                       { Add($1, $3) }
  | expr MINUS expr                      { Sub($1, $3) }
;


basic_expr:
  | INT                                  { N($1) }
  | IDENT                                { Var($1) }
  | LBRACKET RBRACKET                    { Nil }
  | LBRACKET expr_list RBRACKET          { $2 }
  | HD LPAREN expr RPAREN              { Hd($3) }
  | TL LPAREN expr RPAREN              { Tl($3) }
  | CONS LPAREN expr COMMA expr RPAREN   { Cons($3, $5) }
  | LPAREN expr RPAREN                   { $2 }
;

expr_list:
  | expr                                 { Cons($1, Nil) }
  | expr COMMA expr_list                 { Cons($1, $3) }
;
