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
      cmds @ [Urop (Copy, r, rv)]
  | BQuestion b ->
      let cmds, _ = translate_bexp b in
      cmds
  | _ -> failwith "Unsupported command"
  (* TODO: add BQuestion b? *)

(* Translate a MiniImp block to a MiniRISC block *)
let translate_block (BasicBlock (id, commands)) =
  let label = Printf.sprintf "L%d" id in
  let rec translate_commands commands acc =
    match commands with
    | [] -> List.rev acc
    | com :: rest ->
        let translated = translate_com com in
        translate_commands rest (translated @ acc)
  in
  let translated_commands = translate_commands commands [] in
  { label; coms = translated_commands }

(* Translate a MiniImp CFG to a MiniRISC CFG *)
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

(* ************************ PRINT************************** *)
(* let print_block { label; coms } =
  Printf.printf "%s:\n" label;
  List.iter (fun com -> Printf.printf "  %s\n" (string_of_scomm com)) coms

let print_program { blocks; edges; entry; exit } =
  let print_node = function
    | BasicBlock (id, commands) ->
        Printf.printf "Node %d: " id;
        List.iter (fun com -> Printf.printf "%s\t" (string_of_com com)) commands;
        print_newline ()
  in
  let print_edge = function
    | ControlFlow (BasicBlock (id1, _), BasicBlock (id2, _)) ->
        Printf.printf "Edge from Node %d to Node %d\n" id1 id2
  in
  Printf.printf "Control Flow Graph (CFG):\nNodes:\n";
  List.iter print_node cfg.nodes;
  Printf.printf "Edges:\n";
  List.iter print_edge cfg.edges


  Printf.printf "entry: %s\nexit: %s\n" entry exit;
  Printf.printf "blocks:\n";
  List.iter (fun block -> print_block block; print_newline ()) blocks
  Printf.printf "edges:\n";
  List.iter (fun (l1, l2) -> Printf.printf "  %s -> %s\n" l1 l2) edges *)