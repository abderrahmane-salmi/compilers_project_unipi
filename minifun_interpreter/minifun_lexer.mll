{
  open Minifun_parser (* Open the parser module to use the tokens *)
}

let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z']
let ident = alpha (alpha | digit | '_')*

rule read = parse
  | [' ' '\t' '\n'] { read lexbuf } (* Skip whitespace *)
  | "let" { LET }
  | "letfun" { LETFUN }
  | "in" { IN }
  | "if" { IF }
  | "then" { THEN }
  | "else" { ELSE }
  | "fun" { FUN }
  | "true" { BOOL(true) }
  | "false" { BOOL(false) }
  | '+' { PLUS }
  | '-' { MINUS }
  | '*' { TIMES }
  | '<' { LESS }
  | "not" { NOT }
  | "and" { AND }
  | '=' { EQUAL }
  | "=>" { ARROW }
  | '(' { LPAREN }
  | ')' { RPAREN }
  | ident as id { ID(id) }
  | digit+ as num { INT(int_of_string num) }
  | eof { EOF }
  | _ { failwith ("Unexpected character: " ^ Lexing.lexeme lexbuf) }