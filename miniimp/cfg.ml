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
        (* Directly connect the last block of com1 to the first block of com2 *)
        (blocks1 @ blocks2, edges1 @ edges2 @ [
          ControlFlow (List.hd (List.rev blocks1), List.hd blocks2)
        ])
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
        (* Process the loop body (com) to get the blocks and edges *)
        let body_blocks, body_edges = process_com com in

        (* Create a node for the loop condition *)
        let condition_block = create_block (While (b, com)) in

        (* Create a node for the exit block *)
        let exit_block = create_block Skip in

        (* We now define the edges: *)
        let edges = body_edges @ [
          ControlFlow (condition_block, List.hd body_blocks);  (* From loop condition to loop body if condition is true *)
          ControlFlow (condition_block, exit_block);  (* From loop condition to exit if condition is false *)
          ControlFlow (List.hd (List.rev body_blocks), condition_block);  (* From last block in loop body to loop condition *)
        ] in

        (* Return the nodes and edges *)
        (condition_block :: body_blocks @ [exit_block], edges)
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
