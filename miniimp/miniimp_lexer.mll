{
  open Miniimp_parser
  exception LexingError of string
}

rule read = parse
  | [' ' '\t' '\n'] { token lexbuf } (* Skip whitespace *)
  | ['0'-'9']+ as num { NUM (int_of_string num) }
  | ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9']* as id { ID id }
  | ":=" { ASSIGN }
  | ";"  { SEMICOLON }
  | "+"  { PLUS }
  | "-"  { MINUS }
  | "*"  { TIMES }
  | "("  { LPAREN }
  | ")"  { RPAREN }
  | "<"  { LESS }
  | "skip" { SKIP }
  | "if" { IF }
  | "then" { THEN }
  | "else" { ELSE }
  | "while" { WHILE }
  | "do" { DO }
  | eof { EOF }
  | _ { raise (LexingError "Unknown character") }
