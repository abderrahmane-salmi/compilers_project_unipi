%{
  open Ast
%}

%token <string> VAR
%token <int> NUM
%token <bool> BOOL_VALUE
%token DEF MAIN WITH INPUT OUTPUT AS SKIP IF THEN ELSE WHILE DO NOT AND ASSIGN SEMICOLON LPAREN RPAREN LESS PLUS MINUS TIMES EOF TRUE

(* Precedence and Associativity Declarations *)
%nonassoc ELSE

%left PLUS MINUS
%left TIMES

%left AND
%nonassoc NOT

%left DO

%left SEMICOLON

(* Type Declarations *)
%type <Ast.com> com
%type <Ast.aexp> aexp
%type <Ast.bexp> bexp

(* Start Symbol *)
%start program
%type <Ast.program> program

%%

program:
  DEF MAIN WITH INPUT input = VAR OUTPUT output = VAR AS c = com EOF { Main(input, output, c) }

com:
  SKIP { Skip }
| x = VAR ASSIGN a = aexp { Assign(x, a) }
| c1 = com SEMICOLON c2 = com { Seq(c1, c2) }
| IF b = bexp THEN c1 = com ELSE c2 = com { If(b, c1, c2) }
| WHILE b = bexp DO c = com { While(b, c) }
| LPAREN c = com RPAREN { c }

bexp:
| TRUE { Bool true }  
| NOT bexp { Not($2) }
| bexp AND bexp { And($1, $3) }
| aexp LESS aexp { Less($1, $3) }

aexp:
  VAR { Var($1) }
| NUM { Num($1) }
| aexp PLUS aexp { Plus($1, $3) }
| aexp MINUS aexp { Minus($1, $3) }
| aexp TIMES aexp { Times($1, $3) }
| LPAREN a = aexp RPAREN { a }