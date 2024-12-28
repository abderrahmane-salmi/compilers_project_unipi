%{
  open Ast
%}

%token <int> INT
%token <bool> BOOL
%token <string> IDENT
%token EQ ADD SUB MUL LESS AND NOT
%token IF THEN ELSE FUN LET IN LETFUN ARROW
%token LPAREN RPAREN SEMI EOF

(* Precedence and Associativity Declarations *)


(* Type Declarations *)


(* Start Symbol *)
%start program
%type <Ast.expr> program

%%

program:
  | expr EOF { $1 }

expr:
  | INT { IntLit $1 }
  | BOOL { BoolLit $1 }
  | IDENT { Var $1 }
  | expr ADD expr { Op ($1, Add, $3) }
  | expr SUB expr { Op ($1, Sub, $3) }
  | expr MUL expr { Op ($1, Mul, $3) }
  | expr LESS expr { Op ($1, Less, $3) }
  | expr AND expr { Op ($1, And, $3) }
  | NOT expr { Op ($2, Not, $2) }
  | IF expr THEN expr ELSE expr { If ($2, $4, $6) }
  | FUN IDENT ARROW expr { Fun ($2, $4) }
  | LET IDENT EQ expr IN expr { Let ($2, $4, $6) }
  | LETFUN IDENT IDENT EQ expr IN expr { Letfun ($2, $3, $5, $7) }
  | expr expr { App ($1, $2) }
  | LPAREN expr RPAREN { $2 }