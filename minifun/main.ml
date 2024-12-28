open Semantics

let parse_file filename =
  let ic = open_in filename in
  let lexbuf = Lexing.from_channel ic in
  try
    let program = Minifun_parser.program Minifun_lexer.read lexbuf in
    close_in ic;
    program
  with e ->
    close_in ic;
    raise e

let eval_program program =
  let env = [] in  (* Start with an empty environment *)
  let result = eval env program in
  match result with
  | IntVal n -> Printf.printf "Result: %d\n" n
  | BoolVal b -> Printf.printf "Result: %b\n" b
  | _ -> Printf.printf "Invalid result type\n"

let () =
  if Array.length Sys.argv < 2 then
    Printf.printf "Usage: %s <program.mf>\n" Sys.argv.(0)
  else
    let program = parse_file Sys.argv.(1) in
    eval_program program
