open Semantics
open Cfg
open Minirisc
open Defined_variables
open Liveness
open Minirisc_allocator
open Minirisc_optimizer

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

  let minirisc_cfg_str = MiniRISC.string_of_program minirisc_cfg in
  Printf.printf "\nTranslated MiniRISC CFG:\n%s\n" minirisc_cfg_str;

  (* Translate MiniRISC CFG to MiniRISC Code *)
  let minirisc_program = Minirisc_code.translate_cfg_to_program minirisc_cfg in

  let minirisc_program_str = MiniRISC.string_of_program minirisc_program in
  Printf.printf "\nTranslated MiniRISC Program:\n%s\n" minirisc_program_str;

  (* Dataflow Analysis *)
  (* Perform Defined Variables Analysis *)
  let defined_variables_analysis_state = DefinedVariables.defined_variables_analysis minirisc_cfg in
  DefinedVariables.print_defined_variables_analysis_state defined_variables_analysis_state;

  (* Perform Liveness Analysis *)
  let liveness_state = Liveness.liveness_analysis minirisc_cfg in
  Liveness.print_liveness_state liveness_state;

  (* Optimize the CFG by merging registers *)
  let optimized_cfg = MiniriscOptimizer.optimize_registers minirisc_cfg liveness_state in
  let optimized_cfg_str = MiniRISC.string_of_program optimized_cfg in
  Printf.printf "\nOptimized MiniRISC CFG after Register Merging:\n%s\n" optimized_cfg_str;

  (* Apply Register Allocation *)
  let num_registers = 5 in
  let optimized_minirisc_cfg = MiniriscAllocator.apply_register_allocation minirisc_cfg num_registers in
  let optimized_minirisc_cfg_str = MiniRISC.string_of_program optimized_minirisc_cfg in
  Printf.printf "\nOptimized MiniRISC CFG with %d registers:\n%s\n" num_registers optimized_minirisc_cfg_str;

  (* Evaluate the program with the provided input value *)
  let result_env = eval_prg initial_env program input_value in

  (* Print the result (output variable) *)
  let output_value = 
    match lookup result_env "out" with
    | Some value -> value
    | None -> failwith "\nOutput variable not found in the environment"
  in
  Printf.printf "\nOutput: %d\n" output_value
