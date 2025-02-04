%{
  open Ast (* Open the AST module to use the types *)
%}

(* Tokens *)
%token <int> INT
%token <bool> BOOL
%token <string> IDENT
%token LET LETFUN IN IF THEN ELSE FUN
%token PLUS MINUS TIMES LESS NOT AND
%token EQUAL ARROW LPAREN RPAREN
%token EOF

(* Precedence and associativity *)
%nonassoc IN
%nonassoc ELSE
%right ARROW
%left PLUS MINUS
%left TIMES
%left LESS
%left AND

(* Start symbol *)
%start <expr> program

%%

(* A MiniFun program is a single expression *)
program:
  | e = expr; EOF { e }

(* Expressions *)
expr:
  | fun_expr { $1 }
  | let_expr { $1 }
  | if_expr { $1 }
  | binop_expr { $1 }
  | app_expr { $1 }

fun_expr:
  | FUN x = IDENT ARROW e = expr { Fun(x, e) }

let_expr:
  | LET x = IDENT EQUAL t1 = expr IN t2 = expr { Let(x, t1, t2) }
  | LETFUN f = IDENT x = IDENT EQUAL t1 = expr IN t2 = expr { Letfun(f, x, t1, t2) }

if_expr:
  | IF e1 = expr THEN e2 = expr ELSE e3 = expr { If(e1, e2, e3) }

binop_expr:
  | e1 = expr op = binop e2 = expr { Op(e1, op, e2) }

app_expr:
  | app_expr atomic_expr { App($1, $2) }
  | atomic_expr { $1 }

atomic_expr:
  | i = INT { IntLit(i) }
  | b = BOOL { BoolLit(b) }
  | x = IDENT { Var(x) }
  | NOT e = atomic_expr { Op(e, Not, e) } (* Fix NOT ambiguity *)
  | LPAREN e = expr RPAREN { e }

binop:
  | PLUS { Add }
  | MINUS { Sub }
  | TIMES { Mul }
  | LESS { Less }
  | AND { And }
