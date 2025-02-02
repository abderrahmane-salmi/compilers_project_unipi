(* ************************* Target Code Generation ************************* *)
open Minirisc
open MiniRISC
open Liveness
open Liveness

module TargetCodeGeneration = struct

  (* ************************* REGISTER REDUCTION ************************* *)

  (* Reserve 2 registers for temporary values *)
  let rA = 2 (* Temporary register A *)
  let rB = 3 (* Temporary register B *)

  (* Choose the n - 2 most frequently used registers *)
  let choose_registers_to_keep (liveness_state : (string, Liveness.analysis_state) Hashtbl.t) (n : int) : int list =
    (* Count the frequency of each register across all blocks *)
    let register_counts = Hashtbl.create 10 in
    Hashtbl.iter (fun _ state ->
      Liveness.RegisterSet.iter (fun r ->
        if r <> rA && r <> rB then (* Exclude temporary registers *)
          let count = try Hashtbl.find register_counts r with Not_found -> 0 in
          Hashtbl.replace register_counts r (count + 1)
      ) state.in_set
    ) liveness_state;

    (* Sort registers by frequency (most frequent first) *)
    let sorted_registers =
      Hashtbl.fold (fun r count acc -> (r, count) :: acc) register_counts []
      |> List.sort (fun (_, count1) (_, count2) -> compare count2 count1)
      |> List.map fst
    in

    let take n lst =
      let rec aux n acc lst =
        match lst with
        | [] -> List.rev acc
        | x :: xs when n > 0 -> aux (n - 1) (x :: acc) xs
        | _ -> List.rev acc
      in
      aux n [] lst
    in    

    (* Select the top n - 2 registers *)
    (* List.take (n - 2) sorted_registers *)
    take (n - 2) sorted_registers

  (* Create a mapping from registers to memory addresses *)
  let create_memory_mapping (registers_to_spill : int list) : (int, int) Hashtbl.t =
    let addr = Hashtbl.create 10 in
    List.iteri (fun i r -> Hashtbl.add addr r (i * 4)) registers_to_spill; (* Map to memory addresses *)
    addr

  (* ************************* INSTRUCTION TRANSLATION ************************* *)

  (* Translate an instruction to use memory for spilled registers *)
  let translate_instruction (addr : (int, int) Hashtbl.t) (instr : scomm) : scomm list =
    match instr with
    | Brop (op, r1, r2, r3) ->
        let load_r1 = if Hashtbl.mem addr r1 then
          [LoadI (Hashtbl.find addr r1, rA); Load (rA, rA)]
        else [] in
        let load_r2 = if Hashtbl.mem addr r2 then
          [LoadI (Hashtbl.find addr r2, rB); Load (rB, rB)]
        else [] in
        let store_r3 = if Hashtbl.mem addr r3 then
          [LoadI (Hashtbl.find addr r3, rA); Store (r3, rA)]
        else [] in
        load_r1 @ load_r2 @ [Brop (op, (if Hashtbl.mem addr r1 then rA else r1), (if Hashtbl.mem addr r2 then rB else r2), r3)] @ store_r3
    | _ -> [instr] (* Handle other instructions similarly *)

  (* Translate a block to use memory for spilled registers *)
  let translate_block (addr : (int, int) Hashtbl.t) (block : block) : block =
    let translated_coms = List.concat_map (translate_instruction addr) block.coms in
    { block with coms = translated_coms }

  (* Translate the entire CFG to use memory for spilled registers *)
  let translate_cfg (cfg : program) (n : int) : program =
    let liveness_state = Liveness.liveness_analysis cfg in
    let registers_to_keep = choose_registers_to_keep liveness_state n in
    (* Compute registers to spill *)
    let registers_to_spill = List.filter (fun r -> not (List.mem r registers_to_keep)) (List.init n (fun i -> i)) in
    let addr = create_memory_mapping registers_to_spill in
    let translated_blocks = List.map (translate_block addr) cfg.blocks in
    { cfg with blocks = translated_blocks }

end