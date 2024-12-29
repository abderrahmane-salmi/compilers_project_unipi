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
%type <Ast.expr> if_expr

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
  | if_expr { $1 }

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

(* Non-terminal for let bindings *)
let_expr:
  | LET IDENT EQ expr IN expr { Let ($2, $4, $6) }

(* Non-terminal for recursive function definitions *)
letfun_expr:
  | LETFUN f = IDENT x = IDENT EQ t1 = expr IN t2 = expr { Letfun (f, x, t1, t2) }

(* Non-terminal for function applications *)
app_expr:
  | FUN f = IDENT ARROW t = expr n = INT { App (Fun (f, t), IntLit n) }
  | FUN f = IDENT ARROW t = expr x = IDENT { App (Fun (f, t), Var x) }
  | f = IDENT x = IDENT { App (Var f, Var x) }
  | f = IDENT n = INT { App (Var f, IntLit n) }

if_expr:
  | IF expr THEN expr ELSE expr { If ($2, $4, $6) }