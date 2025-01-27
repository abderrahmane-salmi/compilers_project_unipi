{
  open Miniimp_parser
  exception LexingError of string
}

let digit   = ['0'-'9']+
let ident = ['a'-'z' 'A'-'Z' '_']['a'-'z' 'A'-'Z' '0'-'9' '_']*
let bool = ("true"|"false")
let whitespace = [' ' '\t' '\n' '\r']

rule read = parse
  | whitespace { read lexbuf } (* Ignore whitespace *)
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
  | "true" { TRUE }
  | "false" { FALSE }
  | ident as id { VAR id }
  | digit as num { NUM (int_of_string num) }
  | bool as b { BOOL_VALUE (bool_of_string b)}
  | eof { EOF }
  | _ { failwith "Unrecognized character" }