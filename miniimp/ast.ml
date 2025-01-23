type program = Main of string * string * com

(* Variables are represented by strings *)
and var = string

(* Arithmetic expressions *)
and aexp = 
  | Num of int      (* Integer literals *)
  | Var of string      (* Variable references *)
  | Plus of aexp * aexp  (* Addition *)
  | Minus of aexp * aexp (* Subtraction *)
  | Times of aexp * aexp (* Multiplication *)

(* Boolean expressions *)
and bexp = 
  | Bool of bool    (* Boolean literals *)
  | And of bexp * bexp  (* AND operation *)
  | Not of bexp        (* NOT operation *)
  | Less of aexp * aexp (* Less-than comparison *)

(* Commands *)
and com =
  | Skip              (* Skip command does nothing *)
  | Assign of var * aexp  (* Assignment: x := aexp *)
  | Seq of com * com      (* Sequence of commands *)
  | If of bexp * com * com (* If-then-else *)
  | While of bexp * com  (* While loop *)
  | BQuestion of bexp  (* Boolean conditional expression to represent b? -- used in cfg *)