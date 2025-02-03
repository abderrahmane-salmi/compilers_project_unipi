open Liveness
open Liveness
open Minirisc
open MiniRISC

module Optimizer = struct

  module RegisterSet = Set.Make(Int)
  module StringSet = Set.Make(String)

  (* A reg_mapping from an original register (int) to a new merged register (int) *)
  type reg_reg_mapping = (int, int) Hashtbl.t



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

    (* Return the computed live ranges *)
    ranges

  (* Check if two registers can be merged (their live ranges do not overlap) *)
  let can_merge (live_ranges : (int, StringSet.t) Hashtbl.t) (r1 : int) (r2 : int) : bool =
    let range1 = Hashtbl.find live_ranges r1 in
    let range2 = Hashtbl.find live_ranges r2 in
    StringSet.is_empty (StringSet.inter range1 range2)

  (*
    Merge registers that have disjoint live ranges.
    We use a greedy algorithm: iterate over all registers and try to assign them to an already
    created merged register if their live range does not overlap.
    Special registers (r_in and r_out) are not merged.
    Input: A hashtable reg_mapping registers to live range (set of block labels).
    Output: A reg_mapping from original registers to new merged registers.
  *)
  let merge_registers (ranges : (int, StringSet.t) Hashtbl.t) : reg_reg_mapping =
    let reg_mapping = Hashtbl.create 10 in
    (* We will maintain a list of groups; each group is (merged_reg, live_range set) *)
    let groups = ref [] in

    (* Function to check if two live ranges overlap *)
    let ranges_overlap r1_set r2_set =
      not (StringSet.is_empty (StringSet.inter r1_set r2_set))
    in

    (* Process each register from the ranges table *)
    Hashtbl.iter (fun reg live_range ->
      (* Do not merge special registers *)
      if reg = Minirisccfg.r_in || reg = Minirisccfg.r_out then
        Hashtbl.replace reg_mapping reg reg
      else begin
        (* Try to find a group whose live range does not intersect with this register's live range *)
        let rec try_group groups_found =
          match groups_found with
          | [] -> None
          | (merged_reg, group_range) :: rest ->
              if not (ranges_overlap live_range group_range) then
                Some (merged_reg, group_range)
              else
                try_group rest
        in
        match try_group !groups with
        | Some (merged_reg, group_range) ->
            (* Merge this register into the found group *)
            Hashtbl.replace reg_mapping reg merged_reg;
            (* Update the group live range to include this register's live range *)
            let new_range = StringSet.union group_range live_range in
            groups := (merged_reg, new_range) :: (List.filter (fun (mr, _) -> mr <> merged_reg) !groups)
        | None ->
            (* No suitable group found; create a new merged register *)
            let new_merged_reg = reg in
            Hashtbl.replace reg_mapping reg new_merged_reg;
            groups := (new_merged_reg, live_range) :: !groups
      end
    ) ranges;
    reg_mapping

  (* Optimize the CFG by merging registers *)
  let optimize_registers (cfg : MiniRISC.program) (liveness_tbl : (string, Liveness.analysis_state) Hashtbl.t) : MiniRISC.program =
    (* Compute live ranges for each register in the CFG *)
    let live_ranges = compute_live_ranges cfg liveness_tbl in

    (* Print live ranges for debuging *)
    Printf.printf "\nLive ranges:\n";
    Hashtbl.iter (fun r range ->
      Printf.printf "Register %d: " r;
      StringSet.iter (fun edge ->
        Printf.printf "%s, " edge
      ) range;
      Printf.printf "\n"
    ) live_ranges;
    
    (* Compute the register merging reg_mapping *)
    let reg_map = merge_registers live_ranges in

    (* Print register merging mapping for debuging *)
    Printf.printf "\nRegister merging mapping:\n";
    Hashtbl.iter (fun r1 r2 ->
      Printf.printf "Register %d -> %d\n" r1 r2
    ) reg_map;

    (* Function to update a register according to the reg_mapping *)
    let update_reg r =
      Hashtbl.find_opt reg_map r |> Option.value ~default:r
    in

    (* Update an instruction by replacing register operands *)
    let update_instr instr =
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
      | Jump l -> Jump l
      | Nop -> Nop
    in

    (* Update every instruction in every block *)
    let updated_blocks = List.map (fun block ->
      { block with coms = List.map update_instr block.coms }
    ) cfg.blocks in

    { cfg with blocks = updated_blocks }

end