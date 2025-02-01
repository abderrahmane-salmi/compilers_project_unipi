open Minirisc
open MiniRISC

(* ************************* DATA STRUCTIRES ************************* *)

(* Module for working with sets of registers (integers) *)
module RegisterSet = Set.Make(Int)

(* Type representing the analysis state for a block *)
type analysis_state = {
  in_set : RegisterSet.t;  (* Registers live at the entry of the block, aka lvin *)
  out_set : RegisterSet.t; (* Registers live at the exit of the block, aka lvout *)
}



(* ************************* HELPER FUNCTIONS ************************* *)

(* Extract registers used in a MiniRISC instruction *)
let used_registers (instr : scomm) : RegisterSet.t =
  match instr with
  | Brop (_, r1, r2, _) -> RegisterSet.of_list [r1; r2]
  | Biop (_, r1, _, _) -> RegisterSet.singleton r1
  | Urop (_, r1, _) -> RegisterSet.singleton r1
  | Load (r1, _) -> RegisterSet.singleton r1
  | Store (r1, _) -> RegisterSet.singleton r1
  | CJump (r, _, _) -> RegisterSet.singleton r
  | _ -> RegisterSet.empty

(* Extract registers defined in a MiniRISC instruction *)
let defined_registers (instr : scomm) : RegisterSet.t =
  match instr with
  (* These instructions define a register *)
  | Brop (_, _, _, r) | Biop (_, _, _, r) | Urop (_, _, r)
  | LoadI (_, r) | Load (_, r) -> RegisterSet.singleton r
  (* Other instructions do not define a register *)
  | _ -> RegisterSet.empty



(* ************************* INITIALIZATION ************************* *)

(*
  Initialize the analysis state for all blocks:
    - The exit block starts with 'r_out' live.
    - All other blocks start with empty `in` and `out` sets.
  Input: The MiniRISC control flow graph (CFG).
  Output: A hashtable mapping block labels to their initial analysis state.
*)
let init_analysis_state (cfg : program) : (string, analysis_state) Hashtbl.t =
  (* Create a hashtable for analysis states for all blocks *)
  let states = Hashtbl.create (List.length cfg.blocks) in

  List.iter (fun block ->
    (* For each block, compute the initial 'out' set *)
    let initial_out = if block.label = cfg.exit then
      (* if block is final, use the register of the output 'r_out' because it's always used *)
      RegisterSet.singleton Minirisccfg.r_out
    else
      (* otherwise, init to an empty set *)
      RegisterSet.empty in

    (* Add the block's initial state to the states hashtable *)
    Hashtbl.add states block.label { in_set = RegisterSet.empty; out_set = initial_out }
  ) cfg.blocks;
  states



(* ************************* LIVENESS ANALYSIS ************************* *)

(*
  Perform liveness analysis on the CFG by computing the in and out sets for each block until a fixpoint is reached.
  Input: The MiniRISC control flow graph (CFG).
  Output: Hashtable mapping block labels to their final analysis state.
*)
let liveness_analysis (cfg : program) : (string, analysis_state) Hashtbl.t =
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
      
      (* Compute new out set as the union of the in_sets of all successor_blocks *)
      let successor_blocks = List.filter (fun (l1, _) -> l1 = block.label) cfg.edges in
      let new_curr_block_out_set =  if block.label = cfg.exit then
        (* if block is final, use the register of the output 'r_out' because it's always used *)
        (* lucf (lvout(L)) = {out (register for the output)} if L is final *)
        RegisterSet.singleton Minirisccfg.r_out
      else
        (* otherwise: lucf (lvout(L)) = ⋃(L,L′)∈CFG edges dvin(L′) *)
        List.fold_left (fun acc (_, l2) ->
          let succ_state = Hashtbl.find states_tbl l2 in
          RegisterSet.union acc succ_state.in_set
        ) RegisterSet.empty successor_blocks in
      
      (* Update out_set if changed *)
      if not (RegisterSet.equal new_curr_block_out_set current_block_state.out_set) then (
        Hashtbl.replace states_tbl block.label { current_block_state with out_set = new_curr_block_out_set };
        changed := true
      );
      
      (* Compute new in set by processing instructions backward *)
      (* lub(lvin(L)) = {r used in L} ∪ (lvout(L) \ {r defined in L}) *)
      let new_curr_block_in_set = List.fold_right (fun instr live_after ->
        let used_regs = used_registers instr in
        let defined_regs = defined_registers instr in
        let live_before = RegisterSet.union used_regs (RegisterSet.diff live_after defined_regs) in
        live_before
      ) block.coms new_curr_block_out_set in
      
      (* Update in_set if changed *)
      if not (RegisterSet.equal new_curr_block_in_set current_block_state.in_set) then (
        Hashtbl.replace states_tbl block.label { current_block_state with in_set = new_curr_block_in_set };
        changed := true
      )
    ) cfg.blocks
  done;
  states_tbl



(* ************************* PRINT ************************* *)

let print_liveness_state (states_tbl : (string, analysis_state) Hashtbl.t) : unit =
  Printf.printf "\nLiveness Analysis State:\n";
  Hashtbl.iter (fun label state ->
    let in_set_str = RegisterSet.fold (fun r acc -> acc ^ "r" ^ string_of_int r ^ " ") state.in_set "" in
    let out_set_str = RegisterSet.fold (fun r acc -> acc ^ "r" ^ string_of_int r ^ " ") state.out_set "" in
    Printf.printf "Block %s:\n  in:  %s\n  out: %s\n" label in_set_str out_set_str
  ) states_tbl