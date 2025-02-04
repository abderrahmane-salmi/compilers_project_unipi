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
%left NOT
%nonassoc LPAREN

(* Start symbol *)
%start <expr> program

%%

(* Grammar rules *)
program:
  | e = expr; EOF { e }

expr:
  | a = aexp { a }
  | b = bexp { b }
  | FUN; x = IDENT; ARROW; t = expr { Fun(x, t) }
  | LET; x = IDENT; EQUAL; t1 = expr; IN; t2 = expr { Let(x, t1, t2) }
  | LETFUN; f = IDENT; x = IDENT; EQUAL; t1 = expr; IN; t2 = expr { Letfun(f, x, t1, t2) }
  | IF; t1 = bexp; THEN; t2 = expr; ELSE; t3 = expr { If(t1, t2, t3) }
  | t1 = expr; t2 = expr { App(t1, t2) }
  | LPAREN; t = expr; RPAREN { t }

bexp:
  | b = BOOL { BoolLit(b) }
  | NOT b = bexp { Op(b, Not, b) }
  | t1 = bexp AND t2 = bexp { Op(t1, And, t2) }
  | t1 = aexp LESS t2 = aexp { Op(t1, Less, t2) }

aexp:
  | x = IDENT { Var(x) }
  | i = INT { IntLit(i) }
  | t1 = aexp; op = aop; t2 = aexp { Op(t1, op, t2) }
  | LPAREN a = aexp RPAREN { a }

aop:
  | PLUS { Add }
  | MINUS { Sub }
  | TIMES { Mul }