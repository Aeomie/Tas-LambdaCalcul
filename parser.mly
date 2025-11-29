%{
open Ast
%}

%token <int> INT
%token <string> IDENT
%token PLUS MINUS COMMA
%token LPAREN RPAREN LBRACKET RBRACKET
%token ARROW FUN LET IN EQUAL FIX
%token HD TL CONS IFZERO IFEMPTY THEN ELSE
%token DEREF REF ASSIGN UNIT
%token EOF

%left PLUS MINUS
%left ASSIGN
%nonassoc APP

%type <Ast.term> prog
%start prog

%%

prog:
  | expr EOF { $1 }
;

expr:
  | expr PLUS expr      { Add($1, $3) }
  | expr MINUS expr     { Sub($1, $3) }
  | expr ASSIGN expr    { Assign($1, $3) }
  | expr expr %prec APP { App($1, $2) }
  | FUN IDENT ARROW expr    { Abs($2, $4) }
  | LET IDENT EQUAL expr IN expr { Let($2, $4, $6) }
  | FIX LPAREN IDENT ARROW expr RPAREN { Fix($3, $5) }
  | IFZERO expr THEN expr ELSE expr { IfZero($2, $4, $6) }
  | IFEMPTY expr THEN expr ELSE expr { IfEmpty($2, $4, $6) }
  | unary_expr                  { $1 } 

unary_expr:
  | REF unary_expr     { Ref($2) }
  | DEREF unary_expr   { Deref($2) }
  | basic_expr         { $1 }

basic_expr:
  | INT                        { N($1) }
  | IDENT                      { Var($1) }
  | UNIT                       { Unit }
  | LBRACKET RBRACKET           { Nil }
  | LBRACKET expr_list RBRACKET { $2 }
  | HD LPAREN expr RPAREN      { Hd($3) }
  | TL LPAREN expr RPAREN      { Tl($3) }
  | CONS LPAREN expr COMMA expr RPAREN { Cons($3, $5) }
  | LPAREN expr RPAREN         { $2 }

expr_list:
  | expr                       { Cons($1, Nil) }
  | expr COMMA expr_list       { Cons($1, $3) }
;
