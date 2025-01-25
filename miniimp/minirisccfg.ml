open Minirisc
open MiniRISC
open Cfg
open Cfg
open Ast

(* ************************* MiniImp CFG to MiniRISC CFG ************************* *)

(* Registers Storing *)

let r_in = 0  (* Reserved for input var *)
let r_out = 1 (* Reserved for output var *)

let reg_counter = ref 2
let regs_list = Hashtbl.create 10

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

(* Translate an arithmetic expression (aexp) to MiniRISC commands *)
let rec translate_aexp aexp =
  match aexp with
  | Num n ->
      let r = new_reg () in
      [LoadI (n, r)], r
  | Var v ->
      let r = get_register v in
      [], r
  | Plus (a1, Num n) | Plus (Num n, a1) ->
      let cmds1, r1 = translate_aexp a1 in
      let r = new_reg () in
      cmds1 @ [Biop (AddI, r1, n, r)], r
  | Minus (a1, Num n) ->
      let cmds1, r1 = translate_aexp a1 in
      let r = new_reg () in
      cmds1 @ [Biop (SubI, r1, n, r)], r
  | Times (a1, Num n) | Times (Num n, a1) ->
      let cmds1, r1 = translate_aexp a1 in
      let r = new_reg () in
      cmds1 @ [Biop (MultI, r1, n, r)], r
  | Plus (a1, a2) ->
      let cmds1, r1 = translate_aexp a1 in
      let cmds2, r2 = translate_aexp a2 in
      let r = new_reg () in
      cmds1 @ cmds2 @ [Brop (Add, r1, r2, r)], r
  | Minus (a1, a2) ->
      let cmds1, r1 = translate_aexp a1 in
      let cmds2, r2 = translate_aexp a2 in
      let r = new_reg () in
      cmds1 @ cmds2 @ [Brop (Sub, r1, r2, r)], r
  | Times (a1, a2) ->
      let cmds1, r1 = translate_aexp a1 in
      let cmds2, r2 = translate_aexp a2 in
      let r = new_reg () in
      cmds1 @ cmds2 @ [Brop (Mult, r1, r2, r)], r

(* Translate a boolean expression (bexp) to MiniRISC commands *)
let rec translate_bexp bexp =
  match bexp with
  | Bool b ->
      let r = new_reg () in
      [LoadI ((if b then 1 else 0), r)], r
  | And (b1, Bool true) | And (Bool true, b1) ->
      translate_bexp b1
  | And (_, Bool false) | And (Bool false, _) ->
      let r = new_reg () in
      [LoadI (0, r)], r
  | And (b1, b2) ->
      let cmds1, r1 = translate_bexp b1 in
      let cmds2, r2 = translate_bexp b2 in
      let r = new_reg () in
      cmds1 @ cmds2 @ [Brop (And, r1, r2, r)], r
  | Not b ->
      let cmds, r1 = translate_bexp b in
      let r = new_reg () in
      cmds @ [Urop (Not, r1, r)], r
  | Less (a1, a2) ->
      let cmds1, r1 = translate_aexp a1 in
      let cmds2, r2 = translate_aexp a2 in
      let r = new_reg () in
      cmds1 @ cmds2 @ [Brop (Less, r1, r2, r)], r

(* Translate a MiniImp command (com) to MiniRISC commands *)
let translate_com com =
  match com with
  | Skip -> [Nop]
  | Assign (v, aexp) ->
      let cmds, r = translate_aexp aexp in
      let rv = get_register v in
      if rv = r then cmds
      else cmds @ [Urop (Copy, r, rv)]
  | BQuestion b ->
      let cmds, _ = translate_bexp b in
      cmds
  | _ -> failwith "Unsupported command"

(* Translate a MiniImp block to a MiniRISC block *)
let translate_block (BasicBlock (id, commands)) =
  let label = Printf.sprintf "L%d" id in
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