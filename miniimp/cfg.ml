open Ast

(* Type for nodes in the CFG *)
type node =
  | BasicBlock of int * com  (* Node ID and the code (com) associated with it *)

(* Type for edges in the CFG, representing control flow between nodes *)
type edge =
  | ControlFlow of node * node

(* The CFG itself *)
type cfg = {
  nodes : node list;     (* List of nodes (basic blocks) *)
  edges : edge list;     (* List of control flow edges *)
  entry : node;          (* Entry node (initial) *)
  exit : node;           (* Exit node (final) *)
}

(* Generates a unique ID for each node. You can implement your own ID generator. *)
let node_id_counter = ref 0
let new_node_id () =
  let id = !node_id_counter in
  node_id_counter := !node_id_counter + 1;
  id

(* Function to create a new basic block with code *)
let create_block code =
  let id = new_node_id () in
  BasicBlock (id, code)

(* Recursively generate the CFG from the MiniImp command *)
let generate_cfg program =
  let rec process_com com =
    match com with
    | Skip ->
        let block = create_block Skip in
        ([block], [])  (* No outgoing edges for a single Skip block *)
    | Assign (var, aexp) -> 
        let block = create_block (Assign (var, aexp)) in
        ([block], [])  (* No outgoing edges for a single assignment block *)
    | Seq (com1, com2) -> 
        let blocks1, edges1 = process_com com1 in
        let blocks2, edges2 = process_com com2 in
        let final_block = create_block Skip in
        let edges = edges1 @ edges2 @ [
            ControlFlow (List.hd (List.rev blocks1), List.hd blocks2);  (* From last block of com1 to first block of com2 *)
            ControlFlow (List.hd (List.rev blocks2), final_block);     (* From last block of com2 to the final skip block *)
        ] in
        (blocks1 @ blocks2 @ [final_block], edges)
    | If (b, com1, com2) -> 
        let blocks1, edges1 = process_com com1 in
        let blocks2, edges2 = process_com com2 in
        let decision_block = create_block (If (b, com1, com2)) in
        let final_block = create_block Skip in
        let edges = edges1 @ edges2 @ [
          ControlFlow (decision_block, List.hd blocks1);
          ControlFlow (decision_block, List.hd blocks2);
        ] in
        (decision_block :: blocks1 @ blocks2 @ [final_block], edges)
    | While (b, com) -> 
        let blocks, edges = process_com com in
        let loop_block = create_block (While (b, com)) in
        let exit_block = create_block Skip in
        let edges = edges @ [
          ControlFlow (loop_block, List.hd blocks);
          ControlFlow (List.hd blocks, exit_block)
        ] in
        (loop_block :: blocks @ [exit_block], edges)
  in

  (* Start generating CFG from the main program *)
  let blocks, edges = process_com program in
  let entry_block = List.hd blocks in  (* First block is the entry node *)
  let exit_block = List.hd (List.rev blocks) in  (* Last block is the exit node *)
  {
    nodes = blocks;
    edges = edges;
    entry = entry_block;
    exit = exit_block;
  }
