(* This file contains utility functions that convert Abstract Syntax Tree (AST) expressions and
commands into string representations.
Ex: *)

let rec string_of_aexp (a: Ast.aexp) : string =
  match a with
  | Ast.Num n -> string_of_int n
  | Ast.Var v -> v
  | Ast.Plus (a1, a2) -> "(" ^ string_of_aexp a1 ^ " + " ^ string_of_aexp a2 ^ ")"
  | Ast.Minus (a1, a2) -> "(" ^ string_of_aexp a1 ^ " - " ^ string_of_aexp a2 ^ ")"
  | Ast.Times (a1, a2) -> "(" ^ string_of_aexp a1 ^ " * " ^ string_of_aexp a2 ^ ")"
  
let rec string_of_bexp (b: Ast.bexp) : string =
  match b with
  | Ast.Bool b -> string_of_bool b
  | Ast.And (b1, b2) -> "(" ^ string_of_bexp b1 ^ " and " ^ string_of_bexp b2 ^ ")"
  | Ast.Not b -> "not " ^ string_of_bexp b
  | Ast.Less (a1, a2) -> "(" ^ string_of_aexp a1 ^ " < " ^ string_of_aexp a2 ^ ")"
  
let rec string_of_com (com: Ast.com) : string =
  match com with
  | Ast.Skip -> "skip"
  | Ast.Assign (v, a) -> v ^ " := " ^ string_of_aexp a
  | Ast.BQuestion b -> string_of_bexp b ^ "?"
  | Ast.Seq (c1, c2) -> string_of_com c1 ^ " ; " ^ string_of_com c2
  | Ast.If (b, c1, c2) -> "if " ^ string_of_bexp b ^ " then " ^ string_of_com c1 ^ " else " ^ string_of_com c2
  | Ast.While (b, c) -> "while " ^ string_of_bexp b ^ " do " ^ string_of_com c