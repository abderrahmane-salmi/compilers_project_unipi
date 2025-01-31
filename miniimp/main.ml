open Semantics
open Cfg
open Minirisc

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

  (* Read the integer input from the user via standard input *)
  Printf.printf "Enter an integer: ";
  let input_value = int_of_string (read_line ()) in

  (* Initialize an empty environment *)
  let initial_env = [] in

  (* Extract the command (com) from the program (Main) *)
  let Main(_, _, com) = program in

  (* Generate the Control Flow Graph (CFG) for the program (i.e. its com) and print it *)
  let cfg = Cfg.generate_cfg com in
  Cfg.print_cfg cfg;

  (* Translate MiniImp CFG to MiniRISC CFG and print it *)
  let minirisc_cfg = Minirisccfg.translate_cfg cfg in

  (
  (* Perform Defined Variables Analysis *)
  Dataflow.defined_variables_analysis minirisc_cfg;

  let minirisc_cfg_str = MiniRISC.string_of_program minirisc_cfg in
  Printf.printf "\nTranslated MiniRISC CFG:\n%s\n" minirisc_cfg_str
  );

  (* Translate MiniRISC CFG to MiniRISC Code *)
  let minirisc_program = Minirisc_code.translate_cfg_to_program minirisc_cfg in

  let minirisc_program_str = MiniRISC.string_of_program minirisc_program in
  Printf.printf "\nTranslated MiniRISC Program:\n%s\n" minirisc_program_str;

  (* Evaluate the program with the provided input value *)
  let result_env = eval_prg initial_env program input_value in

  (* Print the result (output variable) *)
  let output_value = 
    match lookup result_env "out" with
    | Some value -> value
    | None -> failwith "Output variable not found in the environment"
  in
  Printf.printf "Output: %d\n" output_value
