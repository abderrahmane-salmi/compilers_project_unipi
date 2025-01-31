open Minirisc
open MiniRISC

module LabelSet = Set.Make(String)
module RegisterSet = Set.Make(Int)

type analysis_state = {
  in_set : RegisterSet.t;  (* Registers defined at block entry *)
  out_set : RegisterSet.t; (* Registers defined at block exit *)
}

(* Initialize the analysis state for all blocks *)
let init_analysis_state (cfg : program) : (string, analysis_state) Hashtbl.t =
  let tbl = Hashtbl.create (List.length cfg.blocks) in
  List.iter (fun block ->
    let initial_in = if block.label = cfg.entry then RegisterSet.singleton 0 (* r_in *)
                     else RegisterSet.empty in
    Hashtbl.add tbl block.label { in_set = initial_in; out_set = initial_in }
  ) cfg.blocks;
  tbl

(* Check if a register is used before being defined *)
let check_use_before_def (block : block) (in_set : RegisterSet.t) : unit =
  let rec check_instructions (current_defs : RegisterSet.t) = function
    | [] -> ()
    | instr :: rest ->
        (* Get used registers in the instruction *)
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
        let defined_reg = match instr with
          | Brop (_, _, _, r) | Biop (_, _, _, r) | Urop (_, _, r)
          | LoadI (_, r) | Load (_, r) | Store (_, r) -> Some r
          | _ -> None
        in
        let new_defs = match defined_reg with
          | Some r -> RegisterSet.add r current_defs
          | None -> current_defs
        in
        check_instructions new_defs rest
  in
  check_instructions in_set block.coms

(* Perform defined variables analysis using the greatest fixpoint *)
let defined_variables_analysis (cfg : program) : unit =
  let state_tbl = init_analysis_state cfg in
  let changed = ref true in
  while !changed do
    changed := false;
    List.iter (fun block ->
      let current_state = Hashtbl.find state_tbl block.label in
      (* Compute new_in as the intersection of all predecessors' out sets *)
      let predecessors = List.filter (fun (_, l2) -> l2 = block.label) cfg.edges in
      let new_in = match predecessors with
        | [] -> current_state.in_set
        | _ ->
            List.fold_left (fun acc (l1, _) ->
              let pred_state = Hashtbl.find state_tbl l1 in
              if RegisterSet.is_empty acc then pred_state.out_set
              else RegisterSet.inter acc pred_state.out_set
            ) RegisterSet.empty predecessors
      in
      (* Update in_set if changed *)
      if not (RegisterSet.equal new_in current_state.in_set) then (
        Hashtbl.replace state_tbl block.label { current_state with in_set = new_in };
        changed := true
      );
      (* Compute new_out by processing instructions *)
      let process_instruction defs instr =
        match instr with
        | Brop (_, _, _, r) | Biop (_, _, _, r) | Urop (_, _, r)
        | LoadI (_, r) | Load (_, r) | Store (_, r) ->
            RegisterSet.add r defs
        | _ -> defs
      in
      let new_out = List.fold_left process_instruction new_in block.coms in
      (* Update out_set if changed *)
      if not (RegisterSet.equal new_out current_state.out_set) then (
        Hashtbl.replace state_tbl block.label { current_state with out_set = new_out };
        changed := true
      )
    ) cfg.blocks
  done;
  (* Check for use-before-def errors *)
  List.iter (fun block ->
    let in_set = (Hashtbl.find state_tbl block.label).in_set in
    check_use_before_def block in_set
  ) cfg.blocks