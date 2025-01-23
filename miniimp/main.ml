open Semantics
open Ast
open Cfg

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

(* Main function to read program, generate CFG, and evaluate it *)
let () =
  if Array.length Sys.argv <> 2 then
    begin
      Printf.eprintf "Usage: %s <program.miniimp>\n" Sys.argv.(0);
      exit 1
    end;

  let program_file = Sys.argv.(1) in

  (* Read the MiniImp program from the file *)
  let lexbuf = Lexing.from_channel (open_in program_file) in

  (* Parse the program *)
  let program = try
      Miniimp_parser.program Miniimp_lexer.read lexbuf
    with
    | Miniimp_parser.Error -> 
      Printf.eprintf "Syntax error while parsing %s\n" program_file;
      exit 1
  in

  (* Now we read the integer input from the user via standard input *)
  Printf.printf "Enter an integer: ";
  let input_value = int_of_string (read_line ()) in

  (* Initialize an empty environment *)
  let initial_env = [] in

  (* Extract the command (com) from the program (Main) *)
  let Main(_, _, com) = program in

  (* Generate the Control Flow Graph (CFG) for the program *)
  let cfg = generate_cfg com in

  (* Print the CFG - Nodes and Edges *)
  Printf.printf "Control Flow Graph (CFG):\n";

  (* Print the nodes with their code *)
  Printf.printf "Nodes:\n";
  List.iter (fun node ->
    match node with
    | BasicBlock (id, code) -> 
        Printf.printf "Node %d: %s\n" id (string_of_com code)
  ) cfg.nodes;

  (* Print the edges *)
  Printf.printf "Edges:\n";
  List.iter (fun edge ->
    match edge with
    | ControlFlow (BasicBlock (id1, _), BasicBlock (id2, _)) ->
        Printf.printf "Edge from Node %d to Node %d\n" id1 id2
  ) cfg.edges;

  (* Evaluate the program with the provided input value *)
  let result_env = eval_prg initial_env program input_value in

  (* Print the result (output variable) *)
  let output_value = 
    match lookup result_env "out" with
    | Some value -> value
    | None -> failwith "Output variable not found in the environment"
  in
  Printf.printf "Output: %d\n" output_value
