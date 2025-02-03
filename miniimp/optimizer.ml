open Liveness
open Liveness
open Minirisc
open MiniRISC

module Optimizer = struct

  module RegisterSet = Set.Make(Int)
  module StringSet = Set.Make(String)

  (* A mapping from an original register (int) to a new merged register (int) *)
  type reg_mapping = (int, int) Hashtbl.t

  (* Compute the live ranges for each register *)
  let compute_live_ranges (cfg : program) (liveness_state : (string, Liveness.analysis_state) Hashtbl.t) : (int, StringSet.t) Hashtbl.t =
    (* Create a hashtable to store live ranges for each register *)
  let ranges = Hashtbl.create 10 in
  (* Iterate over each edge in the CFG *)
  List.iter (fun (l_from, l_to) ->
    (* For the edge from l_from to l_to, registers that are live on this edge are exactly
       those in the in_set of the successor block (l_to) *)
    let state = Hashtbl.find liveness_state l_to in
    (* For each register in the live_in set of block l_to, add this edge to its live range *)
    RegisterSet.iter (fun r ->
      let current_range = Hashtbl.find_opt ranges r |> Option.value ~default:StringSet.empty in
      (* Represent the edge as "l_from->l_to" *)
      Hashtbl.replace ranges r (StringSet.add (l_from ^ "->" ^ l_to) current_range)
    ) state.in_set
  ) cfg.edges;

  (* add the remaining registers from all blocks' instructions that were not already added to the ranges list and give them empty range *)
  List.iter (fun block ->
    List.iter (fun instr ->
      let used_regs = invloved_registers instr in
      RegisterSet.iter (fun r ->
        if not (Hashtbl.mem ranges r) then
          Hashtbl.add ranges r StringSet.empty
      ) used_regs
    ) block.coms
  ) cfg.blocks;

  (* Return the computed live ranges *)
  ranges

  (* Check if two registers can be merged (their live ranges do not overlap) *)
  let can_merge (live_ranges : (int, StringSet.t) Hashtbl.t) (r1 : int) (r2 : int) : bool =
    let range1 = Hashtbl.find live_ranges r1 in
    let range2 = Hashtbl.find live_ranges r2 in
    StringSet.is_empty (StringSet.inter range1 range2)

  (* Merge two registers by replacing all occurrences of r2 with r1 *)
  let merge_registers (cfg : program) (r1 : int) (r2 : int) : program =
    let replace_register instr =
      match instr with
      | Brop (op, r, r_, r__) -> 
        let ur1 = if r = r2 then r1 else r in
        let ur2 = if r_ = r2 then r1 else r_ in
        let ur3 = if r__ = r2 then r1 else r__ in
        Brop (op, ur1, ur2, ur3)
      | Biop (op, r, n, r_) -> 
        let ur1 = if r = r2 then r1 else r in
        let ur_ = if r_ = r2 then r1 else r_ in
        Biop (op, ur1, n, ur_)
      | Urop (op, r, r_) -> 
        let ur = if r = r2 then r1 else r in
        let ur_ = if r_ = r2 then r1 else r_ in
        Urop (op, ur, ur_)
      | Load (r, r_) -> 
        let ur = if r = r2 then r1 else r in
        let ur_ = if r_ = r2 then r1 else r_ in
        Load (ur, ur_)
      | LoadI (n, r) ->
        let ur = if r = r2 then r1 else r in
        LoadI (n, ur)
      | Store (r, r_) ->
        let ur = if r = r2 then r1 else r in
        let ur_ = if r_ = r2 then r1 else r_ in
        Store (ur, ur_)
      | CJump (r, l1, l2) ->
        let ur = if r = r2 then r1 else r in
        CJump (ur, l1, l2)
      | _ -> instr
    in

    (* Update all blocks in the CFG *)
    let updated_blocks = List.map (fun block ->
      { block with coms = List.map replace_register block.coms }
    ) cfg.blocks in

    { cfg with blocks = updated_blocks }

  (* Optimize the CFG by merging registers *)
  let optimize_cfg (cfg : program) (liveness_state: (string, analysis_state) Hashtbl.t) : program =
    (* Compute live ranges for each register *)
    let live_ranges = compute_live_ranges cfg liveness_state in

    (* print live ranges for deugging purposes *)
    Printf.printf "\nLive ranges:\n";
    Hashtbl.iter (fun r range ->
      Printf.printf "r%d: " r;
      StringSet.iter (fun edge -> Printf.printf "%s " edge) range;
      Printf.printf "\n"
    ) live_ranges;

    (* Iterate over all pairs of registers and merge them if possible *)
    let registers = Hashtbl.to_seq_keys live_ranges |> List.of_seq in
    let rec merge_all cfg registers =
      match registers with
      | [] -> cfg
      | r1 :: rest ->
          let cfg_ = List.fold_left (fun cfg_acc r2 ->
            if r1 <> r2 && can_merge live_ranges r1 r2 then
              merge_registers cfg_acc r1 r2
            else
              cfg_acc
          ) cfg rest in
          merge_all cfg_ rest
    in

    merge_all cfg registers

end