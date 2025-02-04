(* Abstract syntax tree for MiniFun *)

(* Variable or function names *)
type ident = string

(* Types for expressions *)
type expr = 
  | IntLit of int        (* Integer literal value *)
  | BoolLit of bool        (* Boolean literal value *)
  | Var of ident          (* Variable reference *)
  | Op of expr * op * expr (* Operator application t1 op t2 *)
  | If of expr * expr * expr (* Conditional expression if t1 then t2 else t3 *)
  | Fun of ident * expr   (* Function definition fun x => t *)
  | Let of ident * expr * expr (* Let binding let x = t1 in t2 *)
  | Letfun of ident * ident * expr * expr (* Recursive function definition letfun f x = t1 in t2 *)
  | App of expr * expr    (* Function application t1 t2 *)

(* Operator types for the language *)
and op = 
  | Add
  | Sub
  | Mul
  | Less
  | Not
  | And