%{
  open Ast (* Open the AST module to use the types *)
%}

(* Tokens *)
%token <int> INT
%token <bool> BOOL
%token <string> ID
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
%left NOT
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
  | x = ID { Var(x) }
  | FUN; x = ID; ARROW; t = expr { Fun(x, t) }
  | LET; x = ID; EQUAL; t1 = expr; IN; t2 = expr { Let(x, t1, t2) }
  | LETFUN; f = ID; x = ID; EQUAL; t1 = expr; IN; t2 = expr { Letfun(f, x, t1, t2) }
  | IF; t1 = expr; THEN; t2 = expr; ELSE; t3 = expr { If(t1, t2, t3) }
  | t1 = expr; op = binop; t2 = expr { Op(t1, op, t2) }
  | NOT; t = expr { Op(BoolLit(true), Not, t) } (* Unary NOT *)
  | t1 = expr; t2 = expr { App(t1, t2) }
  | LPAREN; t = expr; RPAREN { t }

binop:
  | PLUS { Add }
  | MINUS { Sub }
  | TIMES { Mul }
  | LESS { Less }
  | AND { And }