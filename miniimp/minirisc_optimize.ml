open Minirisc
open MiniRISC
open Liveness
open Liveness

(* Merge registers with non-overlapping live ranges *)
let merge_registers (cfg : program) (live_tbl : (string, analysis_state) Hashtbl.t) =
  (* Step 1: Collect all registers used in the program *)
  let all_regs =
    List.fold_left (fun acc block ->
      List.fold_left (fun acc instr ->
        let used = Liveness.used_registers instr in
        let defined = Liveness.defined_registers instr in
        RegisterSet.union (RegisterSet.union used defined) acc
      ) acc block.coms
    ) RegisterSet.empty cfg.blocks
  in

  (* Step 2: Compute live ranges for each register *)
  let live_ranges = Hashtbl.create 10 in
  RegisterSet.iter (fun r ->
    (* Collect all edges where the register is live *)
    let edges =
      List.fold_left (fun acc (src, dest) ->
        let src_live_out = (Hashtbl.find live_tbl src).out_set in
        let dest_live_in = (Hashtbl.find live_tbl dest).in_set in
        if RegisterSet.mem r src_live_out || RegisterSet.mem r dest_live_in then
          (src, dest) :: acc
        else acc
      ) [] cfg.edges
    in
    Hashtbl.add live_ranges r edges
  ) all_regs;

  (* Step 3: Find registers to merge (non-overlapping live ranges) *)
  let merged_regs = Hashtbl.create 10 in
  let avoid_merging = [Minirisccfg.r_out] in (* Never merge the output register *)

  RegisterSet.iter (fun r ->
    if not (List.mem r avoid_merging) && not (Hashtbl.mem merged_regs r) then
      (* Find registers with disjoint live ranges *)
      RegisterSet.iter (fun r2 ->
        if r <> r2 && not (List.mem r2 avoid_merging) then
          let r_edges = Hashtbl.find live_ranges r in
          let r2_edges = Hashtbl.find live_ranges r2 in
          (* Check if live ranges are disjoint *)
          let overlap = List.exists (fun e -> List.mem e r2_edges) r_edges in
          if not overlap then (
            (* Merge r2 into r *)
            Hashtbl.add merged_regs r2 r;
            Hashtbl.replace live_ranges r (r_edges @ r2_edges)
          )
      ) all_regs
  ) all_regs;

  (* Step 4: Update all occurrences of merged registers in the CFG *)
  let update_reg r =
    if Hashtbl.mem merged_regs r then Hashtbl.find merged_regs r else r
  in

  let new_blocks = List.map (fun block ->
    let new_coms = List.map (fun instr ->
      match instr with
      | Brop (op, r1, r2, r3) ->
          Brop (op, update_reg r1, update_reg r2, update_reg r3)
      | Biop (op, r1, n, r2) ->
          Biop (op, update_reg r1, n, update_reg r2)
      | Urop (op, r1, r2) ->
          Urop (op, update_reg r1, update_reg r2)
      | Load (r1, r2) ->
          Load (update_reg r1, update_reg r2)
      | LoadI (n, r) ->
          LoadI (n, update_reg r)
      | Store (r1, r2) ->
          Store (update_reg r1, update_reg r2)
      | CJump (r, l1, l2) ->
          CJump (update_reg r, l1, l2)
      | _ -> instr
    ) block.coms in
    { block with coms = new_coms }
  ) cfg.blocks in

  (* Return the optimized CFG *)
  { cfg with blocks = new_blocks }