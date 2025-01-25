module MiniRISC = struct

  (* ************************* MiniRISC Syntax ************************* *)

  (* Basic operations (brop): involves binary operations between registers *)
  type brop = Add | Sub | Mult | And | Less

  (* Immediate operations (biop): involves operations between a register and an immediate value (integer) *)
  type biop = AddI | SubI | MultI | AndI

  (* Unary operations (urop): involves unary operations like negation or copying a register. *)
  type urop = Not | Copy

  (* MiniRISC simple statements *)
  type scomm =
    | Nop                      (* No operation *)
    | Brop of brop * int * int * int (* brop r1 r2 => r3 -> apply binary operation (add, sub, etc) on two registers and save result in r3 *)
    | Biop of biop * int * int * int  (* biop r1 n => r2 -> apply binary operation between an intermediate value integer n and register r1 and save result in r2 *)
    | Urop of urop * int * int (* urop r1 => r2 -> apply unary operation (not, copy) on a register r1 and save result in r2 *)
    | Load of int * int (* load r1 => r2 -> Load a value from memory address stored in r1 into a register r2: r2 := M[r1] *)
    | LoadI of int * int (* loadI n => r -> Load an immediate value n into a register r: r := n *)
    | Store of int * int (* store r1 => r2 -> Store the value of register r1 into the memory address saved in register r2: M[r2] := r1 *)

  (* Jumps and conditional jumps *)
  type jump =
    | Jump of string  (* jump to label l *)
    | CJump of int * string * string (* cjump r l l' -> if the value in r is zero, jump to label l'; otherwise, jump to the label l *)




  (* ************************* MiniRISC Program ************************* *)

  (* Basic block in MiniRISC *)
  type block = {
    label : string;         (* Label identifying the block *)
    coms : scomm list;    (* List of simple commands (instructions) in the block *)
  }

  (* MiniRISC program is a list of blocks *)
  type program = {
    blocks : block list; (* List of labeled blocks *)
    entry : string;      (* Entry block label *)
    exit : string;      (* Entry block label *)
  }




  (* ************************* Debug Util ************************* *)

  (* Convert operations and commands to strings *)
  let string_of_brop = function
    | Add -> "add"
    | Sub -> "sub"
    | Mult -> "mult"
    | And -> "and"
    | Less -> "less"

  let string_of_biop = function
    | AddI -> "addi"
    | SubI -> "subi"
    | MultI -> "multi"
    | AndI -> "andi"

  let string_of_urop = function
    | Not -> "not"
    | Copy -> "copy"

  let string_of_scomm = function
    | Nop -> "noop"
    | Brop (brop, r1, r2, r3) -> 
      (string_of_brop brop) ^ " r" ^ string_of_int r1 ^ " r" ^ string_of_int r2 ^ " => r" ^ string_of_int r3
    | Biop (biop, r1, n, r2) -> 
      (string_of_biop biop) ^ " r" ^ string_of_int r1 ^ " " ^ string_of_int n ^ " => r" ^ string_of_int r2
    | Urop (urop, r1, r2) ->
      (string_of_urop urop) ^ " r" ^ string_of_int r1 ^ " => r" ^ string_of_int r2
    | Load (r1, r2) -> 
      "load r" ^ string_of_int r1 ^ " => r" ^ string_of_int r2
    | LoadI (n, r) -> 
      "loadI " ^ string_of_int n ^ " => r" ^ string_of_int r
    | Store (r1, r2) -> 
      "store r" ^ string_of_int r1 ^ " => r" ^ string_of_int r2
  
  let string_of_jump = function
    | Jump l -> "jump " ^ l
    | CJump (r, l1, l2) -> "cjump r" ^ string_of_int r ^ " " ^ l1 ^ " " ^ l2

  (* Convert a MiniRISC block to a string *)
  let string_of_block { label; coms } =
    let coms_str = String.concat "\n" (List.map string_of_scomm coms) in
    Printf.sprintf "%s:\n%s" label coms_str

  (* Convert a MiniRISC program to a string *)
  let string_of_program { blocks; entry; exit } =
    let blocks_str = String.concat "\n\n" (List.map string_of_block blocks) in
    Printf.sprintf "entry: %s\nexit: %s\n\n%s" entry exit blocks_str
end