open Ast
open Astutil

(* ************************* CFG Data Structure ************************* *)

(* Type for nodes in the CFG -- each node has an id and a code (com) *)
type node =
  | BasicBlock of int * com list

(* Type for edges in the CFG, representing control flow between nodes *)
type edge =
  | ControlFlow of node * node

(* The Control Flow Graph (CFG) itself *)
type cfg = {
  nodes : node list;     (* List of nodes (basic blocks) *)
  edges : edge list;     (* List of control flow edges *)
  entry : node;          (* Entry node (initial) *)
  exit : node;           (* Exit node (final) *)
}



(* ************************* UTIL ************************* *)

(* Generate a unique ID for each node *)

let node_id_counter = ref 0 (* This keeps track of the node count *)

let new_node_id () =
  let id = !node_id_counter in
  node_id_counter := !node_id_counter + 1;
  id
  (* The "!" operator is used to get the current value of the reference "node_id_counter" *)

(* Function to create a new basic block with a unique id and code (passed as param) *)
let create_block code =
  let id = new_node_id () in
  BasicBlock (id, code)



(* ************************* CFG Generation ************************* *)

(* Helper function to combine blocks into a maximal block *)
let rec group_simple_blocks com =
  match com with
  | Seq (Skip, rest) -> 
    let grouped_rest, remaining = group_simple_blocks rest in
    (Skip :: grouped_rest, remaining)
  | Seq (Assign (v, a), rest) ->
    Printf.printf "Grouping: %s\n" (string_of_com (Assign (v, a)));
      let grouped_rest, remaining = group_simple_blocks rest in
      Printf.printf "Grouped: %s\n" (string_of_com (Assign (v, a)));
      (Assign (v, a) :: grouped_rest, remaining)
  | Seq (Seq (c1, c2), rest) -> group_simple_blocks (Seq (c1, Seq (c2, rest))) (* Flatten nested Seqs *)
  | Seq (BQuestion b, rest) -> ([], Seq (BQuestion b, rest)) (* this case will never happen since user won't input b? *)
  | Seq (If (b, c1, c2), rest) -> ([], Seq (If (b, c1, c2), rest)) (* Stop grouping if it's a complex statement *)
  | Seq (While (b, c), rest) -> ([], Seq (While (b, c), rest)) (* Stop grouping if it's a complex statement *)
  | Skip | Assign _ -> ([com], Skip)     (* Return as part of the current block *)
  | BQuestion _ | If _ | While _ -> ([], com) (* Stop grouping for control-flow statements *)

(* Recursively generate the CFG from the MiniImp program (command) *)
let generate_cfg program =
  let rec process_com com =
    match com with
    (* Simple Statements, like skip, assign, seq, b?, have a simple block containing the code (com) and no outgoing edges *)
    | Skip ->
        let block = create_block [Skip] in
        ([block], [])
    | Assign (var, aexp) -> 
        let block = create_block [(Assign (var, aexp))] in
        ([block], [])
    | BQuestion b ->
        let block = create_block [(BQuestion b)] in
        ([block], [])
    | Seq (If (b, c1, c2), rest) ->
      let blocks1, edges1 = process_com (If (b, c1, c2)) in
      let blocks2, edges2 = process_com rest in
      let edges = edges1 @ edges2 @ [
      ControlFlow (List.hd (List.rev blocks1), List.hd blocks2) (* From last block of com1 to first block of com2 *)
      ] in
      (blocks1 @ blocks2, edges)
    | Seq (While (b, c), rest) ->
      let blocks1, edges1 = process_com (While (b, c)) in
      let blocks2, edges2 = process_com rest in
      let edges = edges1 @ edges2 @ [
      ControlFlow (List.hd (List.rev blocks1), List.hd blocks2) (* From last block of com1 to first block of com2 *)
      ] in
      (blocks1 @ blocks2, edges)
    | Seq (c1, c2) -> 
      let grouped, remaining = group_simple_blocks (Seq (c1, c2)) in

      (* print grouped (which is a list of com) and remaining content (which is a com) *)
      Printf.printf "Grouped: ";
      List.iter (fun com -> Printf.printf "%s; " (string_of_com com)) grouped;
      Printf.printf "\nRemaining: %s\n" (string_of_com remaining);


      let block = create_block grouped in
      (* process_com remaining only if remaining is not empty *)
      if remaining = Skip then
        ([block], [])
      else
        let remaining_blocks, remaining_edges = process_com remaining in
        let edges = remaining_edges @ [
        ControlFlow (block, List.hd remaining_blocks) (* From last block of com1 to first block of com2 *)
        ] in
        (block :: remaining_blocks, edges)
    | If (b, com1, com2) -> 
        (* Process and get the blocks and edges of the true and false branches *)
        let blocks1, edges1 = process_com com1 in
        let blocks2, edges2 = process_com com2 in
        
        (* Create a decision block for the if condition *)
        let decision_block = create_block [(BQuestion b)] in
        
        (* Create a final block which is just a skip that both branches will point to *)
        let final_block = create_block [Skip] in
        
        (* If edges *)
        let edges = edges1 @ edges2 @ [
          ControlFlow (decision_block, List.hd blocks1); (* From decision block to true branch *)
          ControlFlow (decision_block, List.hd blocks2); (* From decision block to false branch *)
          ControlFlow (List.hd (List.rev blocks1), final_block); (* From last block in true branch to final block *)
          ControlFlow (List.hd (List.rev blocks2), final_block); (* From last block in false branch to final block *)
        ] in
        
        (* Return the generated nodes and edges *)
        (decision_block :: blocks1 @ blocks2 @ [final_block], edges)
    | While (b, com) -> 
        (* Process the loop body (com) to get the blocks and edges *)
        let body_blocks, body_edges = process_com com in

        (* Create a node for the loop condition *)
        let condition_block = create_block [(BQuestion b)] in

        (* Create an exit block which is just a skip that the loop condition point to in case loop is done *)
        let exit_block = create_block [Skip] in

        (* While edges *)
        let edges = body_edges @ [
          ControlFlow (condition_block, List.hd body_blocks);  (* From loop condition to loop body if condition is true *)
          ControlFlow (condition_block, exit_block);  (* From loop condition to exit if condition is false *)
          ControlFlow (List.hd (List.rev body_blocks), condition_block);  (* From last block in loop body to loop condition *)
        ] in

        (* Return the generated nodes and edges *)
        (condition_block :: body_blocks @ [exit_block], edges)
  in

  (* Start generating CFG from the main program (its com) *)
  let blocks, edges = process_com program in
  
  (* Get the first and last nodes and set them as entry and exit blocks *)
  let entry_block = List.hd blocks in 
  let exit_block = List.hd (List.rev blocks) in

  (* Return the generated CFG *)
  {
    nodes = blocks;
    edges = edges;
    entry = entry_block;
    exit = exit_block;
  }

(* ************************* DEBUG ************************* *)

(* Function to print the CFG for debugging *)
let print_cfg cfg =
  let print_node = function
    | BasicBlock (id, commands) ->
        Printf.printf "Node %d: " id;
        List.iter (fun com -> Printf.printf "%s; " (string_of_com com)) commands;
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