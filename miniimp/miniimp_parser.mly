%{
  open Miniimp
  open MiniImp
%}

%token <int> NUM
%token <string> ID
%token ASSIGN SEMICOLON PLUS MINUS TIMES
%token LPAREN RPAREN LESS SKIP IF THEN ELSE WHILE DO
%token EOF

%type <MiniImp.com> command
%type <MiniImp.aexp> aexp
%type <MiniImp.bexp> bexp

%start <MiniImp.com> program
%%

program:
  | command EOF { $1 }

command:
  | SKIP { Skip }
  | ID ASSIGN aexp SEMICOLON { Assign ($1, $3) }
  | command SEMICOLON command { Seq ($1, $3) }
  | IF bexp THEN command ELSE command { If ($2, $4, $6) }
  | WHILE bexp DO command { While ($2, $4) }

aexp:
  | NUM { Num $1 }
  | ID { Var $1 }
  | aexp PLUS aexp { Plus ($1, $3) }
  | aexp MINUS aexp { Minus ($1, $3) }
  | aexp TIMES aexp { Times ($1, $3) }
  | LPAREN aexp RPAREN { $2 }

bexp:
  | aexp LESS aexp { Less ($1, $3) }
