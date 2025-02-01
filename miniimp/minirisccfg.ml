(* ************************* MiniImp CFG to MiniRISC CFG ************************* *)

open Minirisc
open MiniRISC
open Cfg
open Cfg
open Ast

(* ************************* REGISTERS ************************* *)

(* Reserved registers for special variables (input and output) *)
let r_in = 0
let r_out = 1

(* Register allocation state *)
let reg_counter = ref 2 (* Start from 2 because 0 and 1 are reserved *)
let regs_list = Hashtbl.create 10 (* Map from variable name to register number *)

(* Allocate a new register *)
let new_reg () =
  let r = !reg_counter in
  reg_counter := !reg_counter + 1;
  r

(* Get the register for a variable if it exists already, otherwise allocate a new one *)
let get_register var =
  match var with
  | "in" -> r_in
  | "out" -> r_out
  | _ ->
      if Hashtbl.mem regs_list var then
        Hashtbl.find regs_list var
      else
        let r = new_reg () in
        Hashtbl.add regs_list var r;
        r



(* ************************* TRANSLATION ************************* *)

(* Translate an arithmetic expression (aexp) to MiniRISC commands *)
let rec translate_aexp ?(target=None) aexp =
  match aexp with
  | Num n ->
      (* Load an immediate value into the target register if provided, otherwise allocate a new one *)
      let r = match target with
        | Some t -> t
        | None -> new_reg ()
      in
      [LoadI (n, r)], r
  | Var v ->
      (* Return the register assigned to the variable *)
      let r = get_register v in
      [], r
  | Plus (a1, Num n) | Plus (Num n, a1) ->
      (* Use AddI if one operand is an immediate value *)
      let cmds1, r1 = translate_aexp a1 in
      let r = match target with
        | Some t -> t
        | None -> new_reg ()
      in
      cmds1 @ [Biop (AddI, r1, n, r)], r
  | Minus (a1, Num n) ->
      (* Use SubI if the second operand is an immediate value *)
      let cmds1, r1 = translate_aexp a1 in
      let r = match target with
        | Some t -> t
        | None -> new_reg ()
      in
      cmds1 @ [Biop (SubI, r1, n, r)], r
  | Times (a1, Num n) | Times (Num n, a1) ->
      (* Use MultI if one operand is an immediate value *)
      let cmds1, r1 = translate_aexp a1 in
      let r = match target with
        | Some t -> t
        | None -> new_reg ()
      in
      cmds1 @ [Biop (MultI, r1, n, r)], r
  (* Use Brop (add, sub, mult) for binary operations (+,-,x) between two registers *)
  | Plus (a1, a2) ->
      let cmds1, r1 = translate_aexp a1 in
      let cmds2, r2 = translate_aexp a2 in
      let r = match target with
        | Some t -> t
        | None -> new_reg ()
      in
      cmds1 @ cmds2 @ [Brop (Add, r1, r2, r)], r
  | Minus (a1, a2) ->
      let cmds1, r1 = translate_aexp a1 in
      let cmds2, r2 = translate_aexp a2 in
      let r = match target with
        | Some t -> t
        | None -> new_reg ()
      in
      cmds1 @ cmds2 @ [Brop (Sub, r1, r2, r)], r
  | Times (a1, a2) ->
      let cmds1, r1 = translate_aexp a1 in
      let cmds2, r2 = translate_aexp a2 in
      let r = match target with
        | Some t -> t
        | None -> new_reg ()
      in
      cmds1 @ cmds2 @ [Brop (Mult, r1, r2, r)], r

(* Translate a boolean expression (bexp) to MiniRISC commands *)
let rec translate_bexp ?(target=None) bexp =
  match bexp with
  | Bool b ->
      (* Load 1 for true, 0 for false into the target register if provided, otherwise allocate a new one *)
      let r = match target with
        | Some t -> t
        | None -> new_reg ()
      in
      [LoadI ((if b then 1 else 0), r)], r
  | And (b1, Bool true) | And (Bool true, b1) ->
      (* Simplify AND with true: return the first operand *)
      translate_bexp ~target:target b1
  | And (_, Bool false) | And (Bool false, _) ->
      (* Simplify AND with false: always load 0 *)
      let r = match target with
        | Some t -> t
        | None -> new_reg ()
      in
      [LoadI (0, r)], r
  | And (b1, b2) ->
      (* Use Brop for AND between two registers *)
      let cmds1, r1 = translate_bexp b1 in
      let cmds2, r2 = translate_bexp b2 in
      let r = match target with
        | Some t -> t
        | None -> new_reg ()
      in
      cmds1 @ cmds2 @ [Brop (And, r1, r2, r)], r
  | Not b ->
      (* Apply NOT operation using Urop *)
      let cmds, r1 = translate_bexp b in
      let r = match target with
        | Some t -> t
        | None -> new_reg ()
      in
      cmds @ [Urop (Not, r1, r)], r
  | Less (a1, a2) ->
      (* Use Brop for Less-than operation *)
      let cmds1, r1 = translate_aexp a1 in
      let cmds2, r2 = translate_aexp a2 in
      let r = match target with
        | Some t -> t
        | None -> new_reg ()
      in
      cmds1 @ cmds2 @ [Brop (Less, r1, r2, r)], r

(* Translate a MiniImp command (com) to MiniRISC commands *)
let translate_com com =
  match com with
  | Skip -> [Nop] (* Translate Skip to Nop *)
  | Assign (v, aexp) ->
      (* Compute aexp and store the result in the variable's register *)
      let rv = get_register v in
      let cmds, r = translate_aexp ~target:(Some rv) aexp in
      if rv = r then cmds (* Avoid redundant copy if result and target registers are the same *)
      else cmds @ [Urop (Copy, r, rv)]
  | BQuestion b ->
      (* Compute the result of b? *)
      let cmds, _ = translate_bexp b in
      cmds
  | _ -> failwith "Unsupported command"




(* ************************* CFG TRANSLATION ************************* *)

(* Translate a MiniImp block to a MiniRISC block *)
let translate_block (BasicBlock (id, commands)) =
  let label = Printf.sprintf "L%d" id in
  
  (* Translate all commands in the block *)
  let rec translate_commands commands =
    match commands with
    | [] -> []
    | com :: rest ->
        let translated = translate_com com in
        translated @ translate_commands rest
  in
  let translated_commands = translate_commands commands in
  { label; coms = translated_commands }

(* Translate a MiniImp CFG to a MiniRISC CFG *)
let translate_cfg { nodes; edges; entry; exit } =
  let translated_blocks = List.map translate_block nodes in

  (* Translate edges by mapping block IDs to labels *)
  let translated_edges =
    List.map (fun (ControlFlow(BasicBlock (id1, _), BasicBlock (id2, _))) ->
      (Printf.sprintf "L%d" id1, Printf.sprintf "L%d" id2)
    ) edges
  in
  let entry_label = match entry with BasicBlock (id, _) -> Printf.sprintf "L%d" id in
  let exit_label = match exit with BasicBlock (id, _) -> Printf.sprintf "L%d" id in
  {
    blocks = translated_blocks;
    edges = translated_edges;
    entry = entry_label;
    exit = exit_label;
  }