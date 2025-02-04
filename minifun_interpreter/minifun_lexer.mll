{
  open Minifun_parser (* Open the parser module to use the tokens *)
}

(* Define regular expressions for tokens *)
let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z']
let ident = alpha (alpha | digit | '_')*

rule read = parse
  (* Whitespace and comments *)
  | [' ' '\t' '\n'] { read lexbuf } (* Skip whitespace *)
  | "(*" [^ '*']* "*)" { read lexbuf } (* Skip comments *)

  (* Keywords *)
  | "let" { LET }
  | "letfun" { LETFUN }
  | "in" { IN }
  | "if" { IF }
  | "then" { THEN }
  | "else" { ELSE }
  | "fun" { FUN }
  | "true" { BOOL(true) }
  | "false" { BOOL(false) }

  (* Operators *)
  | '+' { PLUS }
  | '-' { MINUS }
  | '*' { TIMES }
  | '<' { LESS }
  | "not" { NOT }
  | "and" { AND }

  (* Punctuation *)
  | '=' { EQUAL }
  | "=>" { ARROW }
  | '(' { LPAREN }
  | ')' { RPAREN }

  (* Identifiers *)
  | ident as id { ID(id) }

  (* Integer literals *)
  | digit+ as num { INT(int_of_string num) }

  (* End of file *)
  | eof { EOF }

  (* Error handling *)
  | _ { failwith ("Unexpected character: " ^ Lexing.lexeme lexbuf) }