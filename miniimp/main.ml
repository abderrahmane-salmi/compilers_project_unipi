let () = Printf.printf "Started MiniImp Main"

open Miniimp
open MiniImp

let test_program_1 () =
  (* x := 5 ;
     y := x + 2 ;
     z := y * 3
  *)
  Printf.printf "MiniImp Test Program 1:\n";
  let env = [] in
  let program = Seq (
    Assign ("x", Num 5),
    Seq (
      Assign ("y", Plus (Var "x", Num 2)),
      Assign ("z", Times (Var "y", Num 3))
    )
  ) in

  (* Evaluate the program and print the final environment *)
  let final_env = eval_com env program in

  (* Print the values of variables in the final environment *)
  let print_var v =
    match lookup final_env v with
    | Some value -> Printf.printf "%s = %d\n" v value
    | None -> Printf.printf "%s not found\n" v
  in
  print_var "x";
  print_var "y";
  print_var "z"

let test_program_2 () =
  (* Conditional Statement:
  if x < 10 then
    y := 2 ;
    z := y * 5
  else
    y := 3 ;
    z := y * 10
  *)
  Printf.printf "MiniImp Test Program 2:\n";
  let env = [("x", 5)] in (* Create the initial environment with x = 5 *)
  let program = If (
    Less (Var "x", Num 10),
    Seq (
      Assign ("y", Num 2),
      Assign ("z", Times (Var "y", Num 5))
    ),
    Seq (
      Assign ("y", Num 3),
      Assign ("z", Times (Var "y", Num 10))
    )
  ) in

  (* Evaluate the program and print the final environment *)
  let final_env = eval_com env program in

  (* Print the values of variables in the final environment *)
  let print_var v =
    match lookup final_env v with
    | Some value -> Printf.printf "%s = %d\n" v value
    | None -> Printf.printf "%s not found\n" v
  in
  print_var "x";
  print_var "y";
  print_var "z"

let test_program_3 () =
  (* While Loop:
  x := 0 ;
  while x < 3 do
    x := x + 1
  *)
  Printf.printf "MiniImp Test Program 3:\n";
  let env = [("x", 0)] in
  let program = While (
    Less (Var "x", Num 3),
    Assign ("x", Plus (Var "x", Num 1))
  ) in

  (* Evaluate the program and print the final environment *)
  let final_env = eval_com env program in

  (* Print the values of variables in the final environment *)
  let print_var v =
    match lookup final_env v with
    | Some value -> Printf.printf "%s = %d\n" v value
    | None -> Printf.printf "%s not found\n" v
  in
  print_var "x"

let test_program_4 () =
  (* MiniImp Program from the pdf:
   def main with input in output out as
    x := in ;
    out := 0 ;
    while not x < 1 do (
       out := out + x ;
       x := x − 1
    );
  *)
  Printf.printf "MiniImp Test Program 4:\n";
  let env = [("in", 2)] in (* Initialize input variable with value 2 *)
  let program = DefMain (
    "in", "out",  (* input and output variable names *)
    Seq (
      Seq(
        Assign ("x", Var "in"),  (* x := in *)
        Assign ("out", Num 0)   (* out := 0 *)
      ),
      While (
        Not (Less (Var "x", Num 1)), (* while not x < 1 *)
        Seq (
          Assign ("out", Plus (Var "out", Var "x")),  (* out := out + x *)
          Assign ("x", Minus (Var "x", Num 1))  (* x := x - 1 *)
        )
      )
    )
  ) in

  (* Evaluate the program and print the final environment *)
  let final_env = eval_com env program in

  (* Print the values of variables in the final environment *)
  let print_var v =
    match lookup final_env v with
    | Some value -> Printf.printf "%s = %d\n" v value
    | None -> Printf.printf "%s not found\n" v
  in
  print_var "in";
  print_var "x";
  print_var "out"

let () =
  Printf.printf "MiniImp Main:\n";
  test_program_1 ();
  Printf.printf "\n";
  test_program_2 ();
  Printf.printf "\n";
  test_program_3 ();
  Printf.printf "\n";
  test_program_4 ();