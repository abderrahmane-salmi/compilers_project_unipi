%{
  open Ast
%}

%token <string> VAR
%token <int> NUM
%token DEF MAIN WITH INPUT OUTPUT AS SKIP IF THEN ELSE WHILE DO NOT AND TRUE FALSE ASSIGN SEMICOLON LPAREN RPAREN LCURLY RCURLY LESS PLUS MINUS TIMES EOF

(* Precedence and Associativity Declarations *)
%left PLUS MINUS  (* Left-associative for addition and subtraction *)
%left TIMES       (* Left-associative for multiplication *)
%left AND
%right NOT
%left SEMICOLON  (* nonassoc for command sequencing *)

%type <Ast.com> com
%type <Ast.aexp> aexp
%type <Ast.bexp> bexp

%start program
%type <Ast.program> program

%%

program:
  DEF MAIN WITH INPUT input = VAR OUTPUT output = VAR AS c = com EOF { Main(input, output, c) }

com:
  SKIP { Skip }
| x = VAR ASSIGN a = aexp { Assign(x, a) }
| c1 = com SEMICOLON c2 = com { Seq(c1, c2) }
| IF b = bexp THEN LCURLY c1 = com RCURLY ELSE LCURLY c2 = com RCURLY { If(b, c1, c2) }
| WHILE b = bexp DO LCURLY c = com RCURLY { While(b, c) }

// TODO: HOW TO recogize bool values?
bexp:
  NOT bexp { Not($2) }
| bexp AND bexp { And($1, $3) }
| aexp LESS aexp { Less($1, $3) }
| TRUE { Bool(true) }
| FALSE { Bool(false) }

aexp:
  VAR { Var($1) }
| NUM { Num($1) }
| aexp PLUS aexp { Plus($1, $3) }
| aexp MINUS aexp { Minus($1, $3) }
| aexp TIMES aexp { Times($1, $3) }
| LPAREN aexp RPAREN { $2 }