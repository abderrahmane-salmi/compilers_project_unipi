(* let () =
  if Array.length Sys.argv != 2 then
    failwith "Argument MiniFun-program is needed";
  let in_file = open_in Sys.argv.(1) in
  let lexbuf = Lexing.from_channel in_file in
  Miniimp_parser.program Miniimp_lexer.read lexbuf
  (* let program = (Miniimp_parser.program Miniimp_lexer.read lexbuf) in
    print_int (Aexp.eval(program));
    print_newline() *) *)

let () = Printf.printf "Started MiniImp Main"

open Semantics

(* Function to read the content of a file *)
let read_file filename =
  let ic = open_in filename in
  let rec read_lines acc =
    try
      let line = input_line ic in
      read_lines (acc ^ "\n" ^ line)
    with End_of_file ->
      close_in ic;
      acc
  in
  read_lines ""

(* Main function to run the MiniImp interpreter *)
let () =
  (* Ensure a filename is provided *)
  if Array.length Sys.argv <> 2 then
    (print_endline "Usage: miniimp <filename>";
     exit 1);
  
  let filename = Sys.argv.(1) in
  try
    (* Read and parse the MiniImp file *)
    let file_content = read_file filename in
    let lexbuf = Lexing.from_string file_content in
    let ast = Miniimp_parser.program Miniimp_lexer.read lexbuf in

    (* Initialize the environment and evaluate the program *)
    let initial_env = [] in
    let final_env = eval_prg initial_env ast in

    (* Output the final environment *)
    print_endline "Program executed successfully.";
    print_endline "Final environment:";
    List.iter (fun (var, value) -> Printf.printf "%s = %d\n" var value) final_env;

  with
  | Miniimp_lexer.LexingError msg ->
      Printf.eprintf "Lexical error: %s\n" msg;
      exit 1
  | Miniimp_parser.Error ->
      Printf.eprintf "Syntax error\n";
      exit 1
  | Failure msg ->
      Printf.eprintf "Runtime error: %s\n" msg;
      exit 1


(* let parse_file filename =
  let channel = open_in filename in
  let lexbuf = Miniimp_lexer.from_channel channel in
  try
    Miniimp_parser.program Miniimp_lexer.token lexbuf
  with
  | Miniimp_lexer.LexingError msg -> failwith ("Lexer error: " ^ msg)
  | Miniimp_parser.Error -> failwith "Parser error" *)


(* let () = Printf.printf "Started MiniImp Main"

open Miniimp
open MiniImp

let test_program_1 () =
  (* x := 5 ;
     y := x + 2 ;
     z := y * 3
  *)
  Printf.printf "MiniImp Test Program 1:\n";
  let env = [] in
  let program = Seq (
    Assign ("x", Num 5),
    Seq (
      Assign ("y", Plus (Var "x", Num 2)),
      Assign ("z", Times (Var "y", Num 3))
    )
  ) in

  (* Evaluate the program and print the final environment *)
  let final_env = eval_com env program in

  (* Print the values of variables in the final environment *)
  let print_var v =
    match lookup final_env v with
    | Some value -> Printf.printf "%s = %d\n" v value
    | None -> Printf.printf "%s not found\n" v
  in
  print_var "x";
  print_var "y";
  print_var "z"

let test_program_2 () =
  (* Conditional Statement:
  if x < 10 then
    y := 2 ;
    z := y * 5
  else
    y := 3 ;
    z := y * 10
  *)
  Printf.printf "MiniImp Test Program 2:\n";
  let env = [("x", 5)] in (* Create the initial environment with x = 5 *)
  let program = If (
    Less (Var "x", Num 10),
    Seq (
      Assign ("y", Num 2),
      Assign ("z", Times (Var "y", Num 5))
    ),
    Seq (
      Assign ("y", Num 3),
      Assign ("z", Times (Var "y", Num 10))
    )
  ) in

  (* Evaluate the program and print the final environment *)
  let final_env = eval_com env program in

  (* Print the values of variables in the final environment *)
  let print_var v =
    match lookup final_env v with
    | Some value -> Printf.printf "%s = %d\n" v value
    | None -> Printf.printf "%s not found\n" v
  in
  print_var "x";
  print_var "y";
  print_var "z"

let test_program_3 () =
  (* While Loop:
  x := 0 ;
  while x < 3 do
    x := x + 1
  *)
  Printf.printf "MiniImp Test Program 3:\n";
  let env = [("x", 0)] in
  let program = While (
    Less (Var "x", Num 3),
    Assign ("x", Plus (Var "x", Num 1))
  ) in

  (* Evaluate the program and print the final environment *)
  let final_env = eval_com env program in

  (* Print the values of variables in the final environment *)
  let print_var v =
    match lookup final_env v with
    | Some value -> Printf.printf "%s = %d\n" v value
    | None -> Printf.printf "%s not found\n" v
  in
  print_var "x"

let test_program_4 () =
  (* MiniImp Program from the pdf:
   def main with input in output out as
    x := in ;
    out := 0 ;
    while not x < 1 do (
       out := out + x ;
       x := x − 1
    );
  *)
  Printf.printf "MiniImp Test Program 4:\n";
  let env = [("in", 2)] in (* Initialize input variable with value 2 *)
  let program = DefMain (
    "in", "out",  (* input and output variable names *)
    Seq (
      Seq(
        Assign ("x", Var "in"),  (* x := in *)
        Assign ("out", Num 0)   (* out := 0 *)
      ),
      While (
        Not (Less (Var "x", Num 1)), (* while not x < 1 *)
        Seq (
          Assign ("out", Plus (Var "out", Var "x")),  (* out := out + x *)
          Assign ("x", Minus (Var "x", Num 1))  (* x := x - 1 *)
        )
      )
    )
  ) in

  (* Evaluate the program and print the final environment *)
  let final_env = eval_com env program in

  (* Print the values of variables in the final environment *)
  let print_var v =
    match lookup final_env v with
    | Some value -> Printf.printf "%s = %d\n" v value
    | None -> Printf.printf "%s not found\n" v
  in
  print_var "in";
  print_var "x";
  print_var "out"

let () =
  Printf.printf "MiniImp Main:\n";
  test_program_1 ();
  Printf.printf "\n";
  test_program_2 ();
  Printf.printf "\n";
  test_program_3 ();
  Printf.printf "\n";
  test_program_4 (); *)