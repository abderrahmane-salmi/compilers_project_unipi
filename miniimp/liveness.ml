open Minirisc
open MiniRISC

(* Modules for working with sets of labels (strings) and registers (integers) *)
module LabelSet = Set.Make(String)
module RegisterSet = Set.Make(Int)

(* Type representing the analysis state for a block *)
type analysis_state = {
  in_set : RegisterSet.t;  (* Registers live at the entry of the block *)
  out_set : RegisterSet.t; (* Registers live at the exit of the block *)
}

(*
  Initialize the analysis state for all blocks:
    - The exit block starts with 'r_out' live.
    - All other blocks start with empty `in` and `out` sets.
  Input: The MiniRISC control flow graph (CFG).
  Output: A hashtable mapping block labels to their initial analysis state.
*)
let init_analysis_state (cfg : program) : (string, analysis_state) Hashtbl.t =
  let tbl = Hashtbl.create (List.length cfg.blocks) in
  List.iter (fun block ->
    let initial_out = if block.label = cfg.exit then RegisterSet.singleton 1 (* r_out is always live *)
                      else RegisterSet.empty in
    Hashtbl.add tbl block.label { in_set = RegisterSet.empty; out_set = initial_out }
  ) cfg.blocks;
  tbl

(*
  Process an instruction backward to update the liveness sets.
  Input:
    - instr: The instruction to process.
    - live: The current set of live registers.
  Output:
    - The updated set of live registers.
  Description:
    - If the instruction uses a register, add it to the live set.
    - If the instruction defines a register, remove it from the live set.
*)
let process_instruction (instr : scomm) (live : RegisterSet.t) : RegisterSet.t =
  let used_regs = match instr with
    | Brop (_, r1, r2, _) -> [r1; r2]
    | Biop (_, r1, _, _) -> [r1]
    | Urop (_, r1, _) -> [r1]
    | Load (r1, _) -> [r1]
    | CJump (r, _, _) -> [r]
    | _ -> []
  in
  let defined_reg = match instr with
    | Brop (_, _, _, r) | Biop (_, _, _, r) | Urop (_, _, r)
    | LoadI (_, r) | Load (_, r) | Store (_, r) -> Some r (* These instructions define a register *)
    | _ -> None (* Other instructions do not define a register *)
  in
  (* Add used registers to the live set *)
  let live_with_used = List.fold_left (fun acc r -> RegisterSet.add r acc) live used_regs in
  (* Remove defined registers from the live set *)
  match defined_reg with
  | Some r -> RegisterSet.remove r live_with_used
  | None -> live_with_used

(*
  Perform liveness analysis on the CFG.
  Input:
    - cfg: The MiniRISC control flow graph (CFG).
  Output:
    - A hashtable mapping block labels to their final analysis state.
  Description:
    - Computes the `in` and `out` sets for each block until a fixpoint is reached.
*)
let liveness_analysis (cfg : program) : (string, analysis_state) Hashtbl.t =
  let state_tbl = init_analysis_state cfg in
  let changed = ref true in
  while !changed do
    changed := false;
    List.iter (fun block ->
      let current_state = Hashtbl.find state_tbl block.label in
      (* Compute new_out as the union of the in_sets of all successors *)
      let successors = List.filter (fun (l1, _) -> l1 = block.label) cfg.edges in
      let new_out = List.fold_left (fun acc (_, l2) ->
        let succ_state = Hashtbl.find state_tbl l2 in
        RegisterSet.union acc succ_state.in_set
      ) RegisterSet.empty successors in
      (* Update out_set if changed *)
      if not (RegisterSet.equal new_out current_state.out_set) then (
        Hashtbl.replace state_tbl block.label { current_state with out_set = new_out };
        changed := true
      );
      (* Compute new_in by processing instructions backward *)
      let new_in = List.fold_right process_instruction block.coms new_out in
      (* Update in_set if changed *)
      if not (RegisterSet.equal new_in current_state.in_set) then (
        Hashtbl.replace state_tbl block.label { current_state with in_set = new_in };
        changed := true
      )
    ) cfg.blocks
  done;
  state_tbl

(*
  Print the liveness analysis state for debugging.
  Input:
    - state_tbl: A hashtable mapping block labels to their analysis state.
  Output:
    - Prints the `in` and `out` sets for each block.
*)
let print_liveness_state (state_tbl : (string, analysis_state) Hashtbl.t) : unit =
  Printf.printf "\nLiveness Analysis State:\n";
  Hashtbl.iter (fun label state ->
    let in_set_str = RegisterSet.fold (fun r acc -> acc ^ "r" ^ string_of_int r ^ " ") state.in_set "" in
    let out_set_str = RegisterSet.fold (fun r acc -> acc ^ "r" ^ string_of_int r ^ " ") state.out_set "" in
    Printf.printf "Block %s:\n  in:  %s\n  out: %s\n" label in_set_str out_set_str
  ) state_tbl