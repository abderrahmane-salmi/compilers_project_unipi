%{
  open Ast
%}

%token <string> VAR
%token <int> NUM
%token DEF MAIN WITH INPUT OUTPUT AS SKIP IF THEN ELSE WHILE DO NOT AND ASSIGN SEMICOLON LPAREN RPAREN LCURLY RCURLY LESS PLUS MINUS TIMES EOF

(* Precedence and Associativity Declarations *)
%left PLUS MINUS  (* Left-associative for addition and subtraction *)
%left TIMES       (* Left-associative for multiplication *)
%right NOT
%left AND
%nonassoc SEMICOLON  (* nonassoc for command sequencing *)

%start program
%type <Ast.program> program

%%

program:
  DEF MAIN WITH INPUT VAR OUTPUT VAR AS com EOF { Main($5, $7, $9) }

com:
  SKIP { Skip }
| VAR ASSIGN aexp { Assign($1, $3) }
| com SEMICOLON com { Seq($1, $3) }
| IF bexp THEN com ELSE com { If($2, $4, $6) }
| WHILE bexp DO LCURLY com RCURLY { While($2, $5) }

// HOW TO recogize bool values?
bexp:
  bexp AND bexp { And($1, $3) }
| NOT bexp { Not($2) }
| aexp LESS aexp { Less($1, $3) }

aexp:
  VAR { Var($1) }
| NUM { Num($1) }
| aexp PLUS aexp { Plus($1, $3) }
| aexp MINUS aexp { Minus($1, $3) }
| aexp TIMES aexp { Times($1, $3) }
| LPAREN aexp RPAREN { $2 }