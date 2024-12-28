{
  open Minifun_parser
  exception LexingError of string
}

rule read = parse
  | [' ' '\t' '\n'] { read lexbuf } (* Skip whitespace *)
  | ['0'-'9']+ as num { INT (int_of_string num) }
  | "true"          { BOOL true }
  | "false"         { BOOL false }
  | ['a'-'z' 'A'-'Z' '_']['a'-'z' 'A'-'Z' '0'-'9' '_']* as id { IDENT id }
  | "="             { EQ }
  | "+"             { ADD }
  | "-"             { SUB }
  | "*"             { MUL }
  | "<"             { LESS }
  | "&&"            { AND }
  | "not"           { NOT }
  | "if"            { IF }
  | "then"          { THEN }
  | "else"          { ELSE }
  | "fun"           { FUN }
  | "let"           { LET }
  | "in"            { IN }
  | "letfun"        { LETFUN }
  | "=>"            { ARROW }
  | "("             { LPAREN }
  | ")"             { RPAREN }
  | ";"             { SEMI }
  | eof             { EOF }
  | _ as char       { raise (LexingError ("Unexpected character: " ^ String.make 1 char)) }