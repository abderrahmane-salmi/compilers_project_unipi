open Minirisc
open MiniRISC

(*
  Defined Variables Analysis for the MiniRISC CFG.
  The goal of this analysis is to make sure that no register is used before being initialized in the program.
*)
module DefinedVariables = struct

  (* ************************* DATA STRUCTIRES ************************* *)

  (* Module for working with sets of registers (integers) *)
  module RegisterSet = Set.Make(Int)

  (* Type representing the analysis state for a block *)
  type analysis_state = {
    in_set : RegisterSet.t;  (* Registers defined at block entry *)
    out_set : RegisterSet.t; (* Registers defined at block exit *)
  }



  (* ************************* INITIALIZATION ************************* *)
  (*
    Initialize the analysis state for all blocks in the CFG.
    The entry block starts with `r_in` (register 0) defined, while all other blocks start with empty 'in' and 'out' sets.
    Input: The MiniRISC CFG.
    Output: A hashtable mapping block labels to their initial analysis state.
  *)
  let init_analysis_state (cfg : program) : (string, analysis_state) Hashtbl.t =
    (* Create a hashtable for analysis states for all blocks *)
    let states = Hashtbl.create (List.length cfg.blocks) in
    
    List.iter (fun block ->
      (* For each block, compute the initial 'in' set *)
      let initial_in = if block.label = cfg.entry then
        (* if block is initial, use the register of the output 'r_in' because it's always defined *)
        RegisterSet.singleton Minirisccfg.r_in
      else
        (* otherwise, init to an empty set *)
        RegisterSet.empty in
      
      (* Add the block's initial state to the states hashtable *)
      Hashtbl.add states block.label { in_set = initial_in; out_set = RegisterSet.empty }
    ) cfg.blocks;
    states



  (*
    Check if a register is used before being defined in a block.
      - Iterate through the instructions in the block.
      - For each instruction, check if all used registers are defined.
      - Update the set of defined registers as we processe each instruction.
    Input: The block to check AND the set of registers defined at the entry of the block.
    Output: Raises an exception if a register is used before being defined.
  *)
  let check_use_before_def (block : block) (in_set : RegisterSet.t) : unit =
    let rec check_instructions (current_defs : RegisterSet.t) = function
      | [] -> () (* Base case: no more instructions to check *)
      | instr :: rest ->
          (* Get the registers used in the current instruction *)
          let used_regs = match instr with
            | Brop (_, r1, r2, _) -> [r1; r2]
            | Biop (_, r1, _, _) -> [r1]
            | Urop (_, r1, _) -> [r1]
            | Load (r1, _) -> [r1]
            | CJump (r, _, _) -> [r]
            | _ -> []
          in
          
          (* Check if all used registers are defined *)
          List.iter (fun r ->
            if not (RegisterSet.mem r current_defs) then
              failwith (Printf.sprintf "Register r%d used before definition in block %s" r block.label)
          ) used_regs;
          
          (* Update current_defs with defined register *)
          let new_defs = match instr with
            (* These instructions define a register, so add it to the set *)
            | Brop (_, _, _, r) | Biop (_, _, _, r) | Urop (_, _, r)
            | LoadI (_, r) | Load (_, r) | Store (_, r) -> RegisterSet.add r current_defs
            (* No new register is defined, return the current set *)
            | _ -> current_defs
          in

          (* Recursively check the remaining instructions *)
          check_instructions new_defs rest
    in

    (* Start checking instructions with the initial 'in_set' *)
    check_instructions in_set block.coms



  (* ************************* DEFINED VARIABLES ANALYSIS ************************* *)
  (*
    Perform defined variables analysis:
      - Compute the 'in' and 'out' sets for each block until a fixpoint is reached.
      - Check for use-before-def errors in each block.
    Input: The MiniRISC CFG.
    Output: Raises an exception if a register is used before being defined.
  *)
  let defined_variables_analysis (cfg : program) : (string, analysis_state) Hashtbl.t =
    (* Initialize the analysis state for all blocks *)
    let states_tbl = init_analysis_state cfg in
    
    (* Bool to track if any state has changed *)
    let changed = ref true in
    
    (* Iterate until no further changes occur (fixpoint) *)
    while !changed do
      changed := false;
      List.iter (fun block ->
        (* Get the state for the current block from the states table *)
        let current_block_state = Hashtbl.find states_tbl block.label in
        
        (* Compute 'new_curr_block_in_set' *)
        let new_curr_block_in_set = if block.label = cfg.entry then
          (* if block is initial, use the register of the output 'r_in' because it's always defined *)
          (* lucf (dvin(L)) = {in (register for the input)} if L is initial *)
          RegisterSet.singleton Minirisccfg.r_in
        else
          (* for other blocks: ⋂(L′,L)∈CFG edges dvout(L′) *)
          let predecessors = List.filter (fun (_, l2) -> l2 = block.label) cfg.edges in
          match predecessors with
          | [] -> current_block_state.in_set (* No predecessors: use the current in_set *)
          | _ ->
              (* Compute the intersection of all predecessors' out sets *)
              List.fold_left (fun acc (l1, _) ->
                let pred_state = Hashtbl.find states_tbl l1 in
                if RegisterSet.is_empty acc then
                  pred_state.out_set
                else
                  RegisterSet.inter acc pred_state.out_set
              ) RegisterSet.empty predecessors
        in
        
        (* Update 'in_set' if changed *)
        if not (RegisterSet.equal new_curr_block_in_set current_block_state.in_set) then (
          Hashtbl.replace states_tbl block.label { current_block_state with in_set = new_curr_block_in_set };
          changed := true
        );
        
        (* Compute new_curr_block_out_set by processing instructions in curr block *)
        let process_instruction defs instr =
          match instr with
          | Brop (_, _, _, r) | Biop (_, _, _, r) | Urop (_, _, r)
          | LoadI (_, r) | Load (_, r) | Store (_, r) ->
              RegisterSet.add r defs
          | _ -> defs
        in
        let new_curr_block_out_set = List.fold_left process_instruction new_curr_block_in_set block.coms in
        
        (* Update out_set if changed *)
        if not (RegisterSet.equal new_curr_block_out_set current_block_state.out_set) then (
          Hashtbl.replace states_tbl block.label { current_block_state with out_set = new_curr_block_out_set };
          changed := true
        )
      ) cfg.blocks
    done;

    (* Check for use-before-def errors in each block *)
    List.iter (fun block ->
      let in_set = (Hashtbl.find states_tbl block.label).in_set in
      check_use_before_def block in_set
    ) cfg.blocks;

    states_tbl
    


  (* ************************* PRINT ************************* *)

  let print_defined_variables_analysis_state (states_tbl : (string, analysis_state) Hashtbl.t) : unit =
    Printf.printf "\nDefined Variables Analysis State:\n";
    Hashtbl.iter (fun label state ->
      let in_set_str = RegisterSet.fold (fun r acc -> acc ^ "r" ^ string_of_int r ^ " ") state.in_set "" in
      let out_set_str = RegisterSet.fold (fun r acc -> acc ^ "r" ^ string_of_int r ^ " ") state.out_set "" in
      Printf.printf "Block %s:\n  in:  %s\n  out: %s\n" label in_set_str out_set_str
    ) states_tbl

end