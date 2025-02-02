(* ************************* Translating MiniRISC CFG to MiniRISC Code ************************* *)

open Minirisc
open MiniRISC
open Liveness

(* Translate MiniRISC CFG to MiniRISC program *)
let translate_cfg_to_program cfg =
  let translate_block_with_edges block edges =
    let { label; coms } = block in
    (* Find all outgoing edges for this block *)
    let outgoing_edges = List.filter (fun (l1, _) -> l1 = label) edges in
    let outgoing_labels = List.map snd outgoing_edges in
    (* Handle edges and add jumps *)
    let jump_instr =
      match outgoing_labels with
      | [l] -> [Jump l] (* Unconditional jump to the only outgoing label *)
      | [l_true; l_false] -> (* Conditional jump based on condition register *)
        
        (* The condition register is usually the result of the last operation *)
        let cond_register = match List.rev coms with
          (* condition instruction can be a binary operation (less, and) *)
          | Brop (brop, _, _, r) :: _ -> ( 
            (* brop should be either "less" or "and" *)
            if brop = Less || brop = And then r
            else failwith ("Unsupported binary operation for condition: " ^ (string_of_brop brop))
          )
          (* condition instruction can be a unary operation (not) *)
          | Urop (urop, _, r) :: _ -> (
            (* urop should be "not" *)
            if urop = Not then r
            else failwith ("Unsupported unary operation for condition: " ^ (string_of_urop urop))
          )
          (* condition instruction can be an intermediate load in case of bool literals *)
          | LoadI (_, r) :: _ -> r
          (* condition instruction can be an intermediate load in case of num literals *)
          | _ -> failwith ("Cannot determine condition register in block: " ^ label)
        in
        [CJump (cond_register, l_true, l_false)]
      | _ -> [] (* No outgoing edges: no jump needed *)
    in
    (* Combine original commands with jumps *)
    { label; coms = coms @ jump_instr }
  in

  (* Translate all blocks with their respective edges *)
  let translated_blocks =
    List.map (fun block -> translate_block_with_edges block cfg.edges) cfg.blocks
  in

  (* Return the translated MiniRISC program *)
  {
    blocks = translated_blocks;
    edges = cfg.edges; (* Keep edges for debugging purposes *)
    entry = cfg.entry;
    exit = cfg.exit;
  }


(* ************************* Register Allocation ************************* *)
let reduce_registers (cfg : MiniRISC.program) (n : int) =
  (* Step 1: Determine registers to keep and spill *)
  let all_regs =
    List.fold_left (fun acc block ->
      List.fold_left (fun acc instr ->
        let used = Liveness.used_registers instr in
        let defined = Liveness.defined_registers instr in
        Liveness.RegisterSet.union (Liveness.RegisterSet.union used defined) acc
      ) acc block.coms
    ) Liveness.RegisterSet.empty cfg.blocks
    |> Liveness.RegisterSet.elements
  in

  let num_regs = List.length all_regs in
  if num_regs <= n - 2 then cfg (* No reduction needed *)
  else (
    (* Step 2: Choose registers to keep (first n-2) and spill the rest *)
    let _, spill_regs = List.partition (fun r -> r < n - 2) all_regs in

    (* Step 3: Map spilled registers to memory addresses *)
    let addr_map = Hashtbl.create 10 in
    List.iteri (fun i r -> Hashtbl.add addr_map r (i * 4)) spill_regs; (* 4 bytes per spilled register *)

    (* Step 4: Insert load/store instructions for spilled registers *)
    let process_instr instr =
      match instr with
      | Brop (op, r1, r2, r3) ->
          let load_r1 = if List.mem r1 spill_regs then [
            LoadI (Hashtbl.find addr_map r1, n - 2); (* Use rA (n-2) as temp *)
            Load (n - 2, n - 2)
          ] else [] in
          let load_r2 = if List.mem r2 spill_regs then [
            LoadI (Hashtbl.find addr_map r2, n - 1); (* Use rB (n-1) as temp *)
            Load (n - 1, n - 1)
          ] else [] in
          let store_r3 = if List.mem r3 spill_regs then [
            LoadI (Hashtbl.find addr_map r3, n - 2);
            Store (n - 2, n - 2)
          ] else [] in
          load_r1 @ load_r2 @ [Brop (op, (if List.mem r1 spill_regs then n - 2 else r1), (if List.mem r2 spill_regs then n - 1 else r2), r3)] @ store_r3
      (* Similar handling for Biop, Urop, etc. *)
      | _ -> [instr] (* Simplified for brevity *)
    in

    let new_blocks = List.map (fun block ->
      { block with coms = List.flatten (List.map process_instr block.coms) }
    ) cfg.blocks in

    { cfg with blocks = new_blocks }
  )