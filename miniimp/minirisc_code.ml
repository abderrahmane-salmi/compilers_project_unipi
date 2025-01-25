(* ************************* Translating MiniRISC CFG to MiniRISC Code ************************* *)

open Minirisc
open MiniRISC

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
