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
%left PLUS MINUS
%left TIMES
%left LESS
%left AND
%right NOT
%nonassoc LPAREN

(* Start symbol *)
%start <expr> program

%%

(* Grammar rules *)
program:
  | e = expr; EOF { e }

expr:
  | i = INT { IntLit(i) }
  | b = BOOL { BoolLit(b) }
  | x = IDENT { Var(x) }
  | FUN; x = IDENT; ARROW; t = expr { Fun(x, t) }
  | LET; x = IDENT; EQUAL; t1 = expr; IN; t2 = expr { Let(x, t1, t2) }
  | LETFUN; f = IDENT; x = IDENT; EQUAL; t1 = expr; IN; t2 = expr { Letfun(f, x, t1, t2) }
  | IF; t1 = expr; THEN; t2 = expr; ELSE; t3 = expr { If(t1, t2, t3) }
  | t1 = expr; op = binop; t2 = expr { Op(t1, op, t2) }
  | NOT; t = expr { Op(t, Not, t) }
  | t1 = expr; t2 = app_expr { App(t1, t2) } (* Function application *)
  | LPAREN; t = expr; RPAREN { t }

app_expr:
  | i = INT { IntLit(i) }
  | b = BOOL { BoolLit(b) }
  | x = IDENT { Var(x) }
  | LPAREN; t = expr; RPAREN { t }

binop:
  | PLUS { Add }
  | MINUS { Sub }
  | TIMES { Mul }
  | LESS { Less }
  | AND { And }