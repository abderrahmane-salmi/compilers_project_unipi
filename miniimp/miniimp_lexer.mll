{
  open Miniimp_parser
  exception LexingError of string
}

rule read = parse
  | [' ' '\t' '\n' '\r'] { read lexbuf } (* Ignore whitespace *)
  | "def" { DEF }
  | "main" { MAIN }
  | "with" { WITH }
  | "input" { INPUT }
  | "output" { OUTPUT }
  | "as" { AS }
  | "skip" { SKIP }
  | "if" { IF }
  | "then" { THEN }
  | "else" { ELSE }
  | "while" { WHILE }
  | "do" { DO }
  | "not" { NOT }
  | "and" { AND }
  | ":=" { ASSIGN }
  | ";" { SEMICOLON }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "<" { LESS }
  | "+" { PLUS }
  | "-" { MINUS }
  | "*" { TIMES }
  | ['a'-'z']+ as id { VAR id }
  | ['0'-'9']+ as num { NUM (int_of_string num) }
  | "true" { TRUE }
  | "false" { FALSE }
  | eof { EOF }
  | _ { failwith "Unrecognized character" }