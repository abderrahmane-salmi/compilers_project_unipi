open Cfg
open Minirisc
open Defined_variables
open Liveness
open Minirisc_allocator
open Minirisc_optimizer

(* Main function to read program, generate CFG, and compile it *)
let () =
  (* Check if the correct number of arguments is provided *)
  if Array.length Sys.argv <> 6 then
    begin
      Printf.eprintf "Error: incorrect input!\n";
      Printf.eprintf "Correct imput format: <num_registers> <check_undefined_vars> <enable_optimization> <program.miniimp> <output_path>\n";
      Printf.eprintf "  <num_registers>: Number of registers available in the target machine (must be >= 4).\n";
      Printf.eprintf "  <check_undefined_vars>: Enable/disable undefined variable check (true/false).\n";
      Printf.eprintf "  <enable_optimization>: Enable/disable optimization (true/false).\n";
      Printf.eprintf "  <program.miniimp>: Path to the MiniImp program file.\n";
      Printf.eprintf "  <output_path>: Path to write the compiled MiniRISC code.\n";
      exit 1
    end;

  (* Parse command-line arguments *)
  let num_registers = int_of_string Sys.argv.(1) in
  let check_undefined_vars = bool_of_string Sys.argv.(2) in
  let enable_optimization = bool_of_string Sys.argv.(3) in
  let program_file = Sys.argv.(4) in
  let output_path = Sys.argv.(5) in

  (* Validate the number of registers *)
  if num_registers < 4 then
    begin
      Printf.eprintf "Error: Number of registers must be at least 4.\n";
      exit 1
    end;

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

  (* Extract the command (com) from the program (Main) *)
  let Main(_, _, com) = program in

  (* Generate the Control Flow Graph (CFG) for the program (i.e. its com) and print it *)
  let cfg = Cfg.generate_cfg com in
  Cfg.print_cfg cfg;

  (* Translate MiniImp CFG to MiniRISC CFG and print it *)
  let minirisc_cfg = Minirisccfg.translate_cfg cfg in
  let minirisc_cfg_str = MiniRISC.string_of_program minirisc_cfg in
  Printf.printf "\nTranslated MiniRISC CFG:\n%s\n" minirisc_cfg_str;

  (* Perform Defined Variables Analysis if enabled *)
  if check_undefined_vars then
    begin
      let defined_variables_analysis_state = DefinedVariables.defined_variables_analysis minirisc_cfg in
      DefinedVariables.print_defined_variables_analysis_state defined_variables_analysis_state;
    end;

  (* Perform Liveness Analysis *)
  let liveness_state = Liveness.liveness_analysis minirisc_cfg in
  Liveness.print_liveness_state liveness_state;

  (* Optimize the CFG if enabled *)
  let optimized_cfg =
    if enable_optimization then
      begin
        let optimized_cfg = MiniriscOptimizer.optimize_registers minirisc_cfg liveness_state in
        let optimized_cfg_str = MiniRISC.string_of_program optimized_cfg in
        Printf.printf "\nOptimized MiniRISC CFG after Register Merging:\n%s\n" optimized_cfg_str;
        optimized_cfg
      end
    else
      minirisc_cfg
  in

  (* Apply Register Allocation *)
  let allocated_cfg = MiniriscAllocator.apply_register_allocation optimized_cfg num_registers in
  let allocated_cfg_str = MiniRISC.string_of_program allocated_cfg in
  Printf.printf "\nMiniRISC CFG with %d registers:\n%s\n" num_registers allocated_cfg_str;

  (* Translate the allocated MiniRISC CFG to MiniRISC program code *)
  let minirisc_program = Minirisc_code.translate_cfg_to_program allocated_cfg in
  let minirisc_program_str = MiniRISC.string_of_program_no_edges minirisc_program.blocks in
  Printf.printf "\nTranslated MiniRISC Program:\n%s\n" minirisc_program_str;

  (* Write the compiled MiniRISC code to the output file *)
  let output_channel = open_out output_path in
  Printf.fprintf output_channel "%s" minirisc_program_str;
  close_out output_channel;

  Printf.printf "\nCompilation successful. MiniRISC code written to %s\n" output_path