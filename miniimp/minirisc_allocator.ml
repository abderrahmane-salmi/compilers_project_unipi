open Minirisc
open MiniRISC

(* ************************* REGISTER ALLOCATION ************************* *)

(* Register allocator for a fixed number of registers (n ≥ 4) *)

(* Function to allocate registers dynamically *)
let allocate_registers (n: int) (cfg: program) : program =
  if n < 4 then failwith "Number of registers must be at least 4";

  (* Mapping of virtual registers to physical registers *)
  let reg_map = Hashtbl.create 10 in

  (* List of available registers (excluding r_in and r_out) *)
  let available_regs = ref (List.init (n - 2) (fun i -> i + 2)) in

  (* Helper function to allocate a register for a variable *)
  let get_register r =
    if Hashtbl.mem reg_map r then
      Hashtbl.find reg_map r
    else
      match !available_regs with
      | reg :: rest ->
          available_regs := rest;
          Hashtbl.add reg_map r reg;
          reg
      | [] ->
          failwith ("No available registers for r" ^ string_of_int r) (* Spilling would be needed here *)
  in

  (* Process each block and remap registers *)
  let remap_block block =
    let remap_instr instr =
      match instr with
      | Brop (op, r1, r2, r3) -> Brop (op, get_register r1, get_register r2, get_register r3)
      | Biop (op, r1, imm, r2) -> Biop (op, get_register r1, imm, get_register r2)
      | Urop (op, r1, r2) -> Urop (op, get_register r1, get_register r2)
      | Load (r1, r2) -> Load (get_register r1, get_register r2)
      | Store (r1, r2) -> Store (get_register r1, get_register r2)
      | CJump (r, l1, l2) -> CJump (get_register r, l1, l2)
      | _ -> instr
    in
    { block with coms = List.map remap_instr block.coms }
  in

  (* Apply register allocation to all blocks *)
  { cfg with blocks = List.map remap_block cfg.blocks }
