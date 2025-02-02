open Minirisc
open MiniRISC
open Liveness
open Liveness

module RegisterAllocation = struct
  (* Module for working with sets of registers *)
  module RegisterSet = Set.Make(Int)

  (* Type representing register allocation state *)
  type allocation_state = {
    keep : RegisterSet.t; (* Registers that stay in registers *)
    spill : (int, int) Hashtbl.t; (* Registers that are spilled to memory, mapped to memory addresses *)
  }

  (* Reserve 2 registers for temporary values. PS: 2 and 3 because 0 and 1 are for in and out *)
  let rA = 101
  let rB = 102

  (* Function to count register usage frequency in the program *)
  let count_register_usage (cfg : program) : (int, int) Hashtbl.t =
    let freq_table = Hashtbl.create 10 in
    let update_freq r =
      Hashtbl.replace freq_table r (1 + (Hashtbl.find_opt freq_table r |> Option.value ~default:0))
    in
    List.iter (fun block ->
      List.iter (fun instr ->
        let used = used_registers instr in
        let defined = defined_registers instr in
        (* get the union of defined and used registers (no duplicates tho) *)
        let used_regs = RegisterSet.union used defined in
        RegisterSet.iter update_freq used_regs
      ) block.coms
    ) cfg.blocks;
    freq_table

  (* Allocate n registers while spilling the rest into memory *)
  let allocate_registers (cfg : program) (n : int) : allocation_state =
    if n < 4 then failwith "Number of registers must be at least 4";

    let freq_table = count_register_usage cfg in
    let sorted_regs = List.sort (
      fun r1 r2 -> compare (Hashtbl.find freq_table r2) (Hashtbl.find freq_table r1)
    ) (Hashtbl.to_seq_keys freq_table |> List.of_seq) in
    let keep_regs = List.fold_left (fun acc r ->
      if List.length acc < n - 4 then
        (* special registers: r_in (0), r_out(1), rA(101), rB(102) are kept by default, so ignore them here *)
        if r <> Minirisccfg.r_in && r <> Minirisccfg.r_out && r <> rA && r <> rB then
          r :: acc
        else
          acc
      else
        acc
    ) [] sorted_regs in
    (* add special registers to the keep_regs list *)
    let keep_regs = [Minirisccfg.r_in; Minirisccfg.r_out; rA; rB] @ keep_regs in
    (* convert keep_regs to a set *)
    let keep_set = RegisterSet.of_list keep_regs in

    (* Create a mapping of spilled registers to memory addresses *)
    let spill_table = Hashtbl.create 10 in
    let spill_addr = ref 8001 in

    List.iter (fun r ->
      (* if the register is not in the keep set, add it to the spill table *)
      if not (RegisterSet.mem r keep_set) then begin
        Hashtbl.add spill_table r !spill_addr;
        spill_addr := !spill_addr + 1;
      end
    ) sorted_regs;

    (* Return the allocation state *)
    { keep = keep_set; spill = spill_table }

  (* Translate a single instruction to handle register spills *)
  let translate_instruction (alloc_state : allocation_state) (instr : scomm) : scomm list =
    let load_spilled r target =
      if Hashtbl.mem alloc_state.spill r then
        let addr = Hashtbl.find alloc_state.spill r in
        [LoadI (addr, target); Load (target, target)]
      else []
    in
    (*
      spilled_r: register that is spilled
      value_r: register that contains result value that we want to store in memory
      mem_r: register that will contain memory address for spilled_r
    *)
    let store_spilled spilled_r value_r mem_r =
      if Hashtbl.mem alloc_state.spill spilled_r then
        let addr = Hashtbl.find alloc_state.spill spilled_r in
        [LoadI (addr, mem_r); Store (value_r, mem_r)]
      else []
    in

    match instr with
    | Brop (op, r1, r2, r3) ->
        let load1 = load_spilled r1 rA in
        let load2 = load_spilled r2 rB in
        let store = store_spilled r3 rB rA in
        let r1_ = if Hashtbl.mem alloc_state.spill r1 then rA else r1 in
        let r2_ = if Hashtbl.mem alloc_state.spill r2 then rB else r2 in
        load1 @ load2 @ [Brop (op, r1_, r2_, rB)] @ store
    | Biop (op, r1, n, r2) ->
        let load = load_spilled r1 rB in
        let store = store_spilled r2 rB rA in
        let r1_ = if Hashtbl.mem alloc_state.spill r1 then rB else r1 in
        let r2_ = if Hashtbl.mem alloc_state.spill r2 then rB else r2 in
        load @ [Biop (op, r1_, n, r2_)] @ store
    | Urop (op, r1, r2) ->
        let load = load_spilled r1 rB in
        let store = store_spilled r2 rB rA in
        let r1_ = if Hashtbl.mem alloc_state.spill r1 then rB else r1 in
        let r2_ = if Hashtbl.mem alloc_state.spill r2 then rB else r2 in
        load @ [Urop (op, r1_, r2_)] @ store
    | LoadI (n, r) ->
      if Hashtbl.mem alloc_state.spill r then
        let addr_r = Hashtbl.find alloc_state.spill r in
        [LoadI (n, rB); LoadI (addr_r, rA); Store (rB, rA)]
      else
        [instr]
    | CJump (r, l1, l2) ->
        let load = load_spilled r rA in
        let r_ = if Hashtbl.mem alloc_state.spill r then rA else r in
        load @ [CJump (r_, l1, l2)]
    | _ -> [instr]

  (* Apply register allocation to an entire MiniRISC CFG *)
  let apply_register_allocation (cfg : program) (n : int) : program =
    let alloc_state = allocate_registers cfg n in

    (* print keep and spell sets for debuging *)
    Printf.printf "\nAllocation state:\n";
    print_endline "Keep set:";
    RegisterSet.iter (fun r -> print_int r; print_string " ") alloc_state.keep;
    print_endline "";
    print_endline "Spill table:";
    Hashtbl.iter (fun r addr -> print_int r; print_string " -> "; print_int addr; print_string ", ") alloc_state.spill;
    print_endline "";

    let translated_blocks = List.map (fun block ->
      { block with coms = List.flatten (List.map (translate_instruction alloc_state) block.coms) }
    ) cfg.blocks in
    { cfg with blocks = translated_blocks }

end
