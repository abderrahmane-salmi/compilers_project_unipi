%{
  open Ast
%}

%token <int> INT
%token <bool> BOOL
%token <string> IDENT
%token EQ PLUS MINUS TIMES LESS AND NOT
%token IF THEN ELSE FUN LET IN LETFUN ARROW
%token LPAREN RPAREN EOF

(* Precedence and Associativity Declarations *)

%left PLUS MINUS
%left TIMES

%left AND
%nonassoc NOT

(* Type Declarations *)
%type <Ast.expr> expr
%type <Ast.expr> aexp
%type <Ast.expr> bexp
%type <Ast.expr> let_expr
%type <Ast.expr> letfun_expr
%type <Ast.expr> app_expr

(* Start Symbol *)
%start program
%type <Ast.expr> program

%%

(* Program: top-level entry point *)
program:
  | expr EOF { $1 }

(* Expression can be either an arithmetic or boolean expression or function app *)
expr:
  | aexp { $1 }
  | bexp { $1 }
  | let_expr { $1 }
  | letfun_expr { $1 }
  | app_expr { $1 }

(* Non-terminal for arithmetic expressions (aexp) *)
aexp:
  | INT { IntLit $1 }
  | IDENT { Var $1 }
  | aexp PLUS aexp { Op ($1, Add, $3) }
  | aexp MINUS aexp { Op ($1, Sub, $3) }
  | aexp TIMES aexp { Op ($1, Mul, $3) }
  | LPAREN aexp RPAREN { $2 }

(* Non-terminal for boolean expressions (bexp) *)
bexp:
  | BOOL { BoolLit $1 }
  | bexp AND bexp { Op ($1, And, $3) }
  | NOT bexp { Op ($2, Not, $2) }
  | aexp LESS aexp { Op ($1, Less, $3) }
  | IF bexp THEN expr ELSE expr { If ($2, $4, $6) }

(* Non-terminal for let bindings *)
let_expr:
  | LET IDENT EQ expr IN expr { Let ($2, $4, $6) }

(* Non-terminal for recursive function definitions *)
letfun_expr:
  | LETFUN IDENT IDENT EQ expr IN expr { Letfun ($2, $3, $5, $7) }

(* Non-terminal for function applications *)
app_expr:
  | FUN x = IDENT ARROW t = expr { Fun (x, t) }
  | app_expr INT { App ($1, IntLit $2) }
  | app_expr IDENT { App ($1, Var $2) }