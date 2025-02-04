module MiniTyFun = struct

  (* --------------- Abstract syntax tree --------------- *)

  (* Variable or function names *)
  type ident = string

  (* Supported Types for MiniTyFun *)
  and typ = 
    | TInt 
    | TBool
    | TFun of typ * typ  (* Function type τ -> τ' *)

  (* Types for expressions *)
  type expr =
    | IntLit of int
    | BoolLit of bool
    | Var of ident
    | Op of expr * op * expr
    | If of expr * expr * expr
    | Fun of ident * typ * expr  (* Added a type for the parameter: fun x:τ => t *)
    | Let of ident * expr * expr
    | Letfun of ident * ident * typ * expr * expr  (* Added type for the parameter: letfun f x:τ = t in t *)
    | App of expr * expr

  (* Operator types for the language *)
  and op =
    | Add
    | Sub
    | Mul
    | Less
    | Not
    | And

  (* --------------- Environments --------------- *)

  (* Values Environment: maps variables/functions to their values *)
  (* PS: i use 'a instead of "value" here because value depends on env, so i can't declare it above *)
  type 'a env = (ident * 'a) list

  (* Types Environment: maps variables/functions to their types  *)
  type envtyp = (ident * typ) list

  (* TODO: check if i can use these funs for both envs because they look generic funs *)
  
  (* Look up a variable in the values environment *)
  let rec lookup env x = 
    try List.assoc x env with Not_found -> failwith ("Unbound variable: " ^ x)
  
  (* Update the values environment by adding a new value (var or fun) *)
  let extend env x v = (x, v) :: env

  (* Look up a variable type in the types environment *)
  let rec lookup_typ envtyp x = 
    try List.assoc x envtyp with Not_found -> failwith ("Unbound variable: " ^ x)
  
  (* Update the types environment by adding a new type (var or fun) *)
  let extend_typ envtyp x v = (x, v) :: envtyp

  (* Types for values *)
  type value = 
   | IntVal of int           (* Integer value *)
   | BoolVal of bool         (* Boolean value *)
   | Closure of ident * expr * value env  (* Closure: (x, t, env) *)
   | RecClosure of ident * ident * expr * value env  (* Recursive closure: (f, x, t, env) *)
  (* PS: f = fun name; x = param; t = fun body; env = enviorment *)

  (* --------------- Evaluation --------------- *)

  (* Evaluate expressions *)
  let rec eval env = function
    (* Literals (int and bool): just return their values. *)
    | IntLit i -> IntVal i
    | BoolLit b -> BoolVal b

    (* Variables: look them up in the environment. *)
    | Var x -> lookup env x

    (* Operations: performe arithmetic and logical operations. *)
    | Op (t1, op, t2) ->
      (* t1 op t2 *)
      let v1 = eval env t1 in
      let v2 = eval env t2 in

      (* Make sure the operator exists and it's applied to the correct values
      ex: we can't add a bool with an int. *)
      (match op, v1, v2 with
        | Add, IntVal n1, IntVal n2 -> IntVal (n1 + n2)
        | Sub, IntVal n1, IntVal n2 -> IntVal (n1 - n2)
        | Mul, IntVal n1, IntVal n2 -> IntVal (n1 * n2)
        | Less, IntVal n1, IntVal n2 -> BoolVal (n1 < n2)
        | And, BoolVal b1, BoolVal b2 -> BoolVal (b1 && b2)
        | Not, BoolVal b, _ -> BoolVal (not b)
        | _ -> failwith "Invalid operation")

    (* Conditionals: evaluate the correct branche based on the condition. *)
    | If (t1, t2, t3) ->
      (* if t1 then t2 else t3 *)
      let v1 = eval env t1 in
        (match v1 with
          | BoolVal true -> eval env t2
          | BoolVal false -> eval env t3
          | _ -> failwith "Condition must be a boolean")

    (* Function: create closures. fun x:typ_x => t *)
    | Fun (x, _, t) -> Closure (x, t, env)

    (* Let bindings: evaluate the bound term and then evaluate the body with the updated environment. *)
    | Let (x, t1, t2) ->
      (* let x = t1 in t2 *)
      let v1 = eval env t1 in
      let env_ = extend env x v1 in (* update the value of x with the value of t1 in the env*)
      eval env_ t2 (* use the updated env to evaluate t2 *)

    (* Recursive functions: create recursive closures and evaluate the function body in the extended environment. *)
    | Letfun (f, x, _, t1, t2) ->
      (* letfun f x:typ_x = t1 in t2 *)
      (* TODO: make sure using "rec" here is correct, otherwise remove it *)
      let rec closure = RecClosure (f, x, t1, env) in
        let env_ = extend env f closure in
          eval env_ t2

    (* Applications: apply a function to an argument. t1 t2 *)
    | App (t1, t2) ->
      let v1 = eval env t1 in
      let v2 = eval env t2 in
      (* PS: f = fun name; x = param; t = fun body; env = enviornment *)
      (match v1 with
        | Closure (x, t, closure_env) -> 
            let extended_env = extend closure_env x v2 in
            eval extended_env t
        | RecClosure (f, x, t, closure_env) ->
            let extended_env = extend (extend closure_env x v2) f (RecClosure (f, x, t, closure_env)) in
            eval extended_env t
        | _ -> failwith "Application to non-function")
  

  (* --------------- Type checking --------------- *)

  (* Function to check types *)
  let rec type_of envtyp = function
    (* Literals (int and bool): just return their types *)
    | IntLit _ -> Some TInt
    | BoolLit _ -> Some TBool

    (* Variables: look them up in the environment *)
    | Var x -> Some (lookup_typ envtyp x)

    (* Operations: ensure operands are compatible and return the correct type, t1 op t2 *)
    | Op (t1, op, t2) -> (
      match (type_of envtyp t1, type_of envtyp t2, op) with
        | Some TInt, Some TInt, (Add | Sub | Mul) -> Some TInt (* int (+,-,x) int -> int *)
        | Some TInt, Some TInt, Less -> Some TBool (* int < int -> bool *)
        | Some TBool, Some TBool, And -> Some TBool (* bool && bool -> bool *)
        | Some TBool, _, Not -> Some TBool (* !bool -> bool TODO: make sure its correct, because this is: t1 not _, is it written correctly?*)
        | _ -> None (* Incompatible types *)
    ) 

    (* Conditionals: check if condition is bool, and branches are compatible, if t1 then t2 else t3 *)
    | If (t1, t2, t3) -> (
      match type_of envtyp t1 with
          | Some TBool -> 
              let t2_typ = type_of envtyp t2 in
                if t2_typ = type_of envtyp t3 then
                    t2_typ (* Both branches have the same type, so return it *)
                  else
                    None
          | _ -> None (* Condition is not a boolean *)
    )

    (* Function: create function type with parameter and body types, fun x:typ_x => t *)
    | Fun (x, typ_x, t) -> (
      let envtyp_ = extend_typ envtyp x typ_x in (* Add the param x and its type to the env *)
      match type_of envtyp_ t with (* Check the type of the function body t *)
          | Some typ_t -> Some (TFun (typ_x, typ_t)) (* Return the function type with input and output types *)
          | None -> None
    )

    (* Let bindings: check type of expression and extend environment, let x = t1 in t2 *)
    | Let (x, t1, t2) -> (
      match type_of envtyp t1 with
          | Some typ_x -> 
            let envtyp_ = extend_typ envtyp x typ_x in
              type_of envtyp_ t2
          | None -> None
    )

    (* Recursive function definitions: create function types with recursion *)
    | Letfun (f, x, typ_f, t1, t2) ->(
      (* letfun f x:typ_f = t1 in t2 --- typ_f = TFun(input type, output type) *)
      match typ_f with
      | TFun (typ_input, typ_output) -> (* extract the input and output types *)
        let envtyp_ = extend_typ envtyp x typ_input in (* Add the param x and its type to the env *)
        let envtyp__ = extend_typ envtyp_ f typ_f in (* Add the function f and its type to the env *)
        
        (match type_of envtyp__ t1 with
        (* Check if the fun body t1 has the correct type (same as ourput) *)
        | Some typ_t1 when typ_t1 = typ_output -> type_of envtyp__ t2 (* Return the type of t2 (deduction sys says so) *)
        | _ -> None (* t1 doesn't have the correct type as the fun's output type *))
      | TInt -> None
      | TBool -> None
      (* didin't use (_ -> None) to avoid a warning *)
    )


    (* old solution that doesn't work because circular dependency
      (* Determine the return type of the function by looking at the body (t1) *)
      let envtyp_ = extend_typ envtyp x typ_x in (match type_of envtyp_ t1 with
    | Some typ_y ->  (* typ_y is the type of the function's return value *)
        let fun_type = TFun (typ_x, typ_y) in  (* The function takes typ_x and returns typ_y *)
        let env' = (f, fun_type) :: envtyp_ in
        (match type_of env' t2 with
        | Some _ -> Some fun_type  (* The return type is the function type itself, so return fun_type *)
        | None -> None)
    | None -> None)  (* If t1 cannot be typed, the whole function is not well-typed *)
    *)

    (* Function applications: check if function type matches with argument type, t1 t2 *)
    | App (t1, t2) -> (
      match type_of envtyp t1 with
      | Some (TFun (typ_in, typ_out)) -> (
          match type_of envtyp t2 with
          | Some typ_t2 when typ_t2 = typ_in -> Some typ_out
          | _ -> None)
      | _ -> None)
end

(* ----------------- Testing the code ----------------- *)

open MiniTyFun

let test_program_1 () =
  Printf.printf "Program 1:\t";
  (* let x = 10 in x + 5 *)
  let program = Let (
    "x",
    IntLit 10,
    Op (Var "x", Add, IntLit 5)
  ) in
  match type_of [] program with
  | Some TInt -> (
    Printf.printf "Program is well-typed: int\t";
    match eval [] program with
    | IntVal result -> Printf.printf "Result: %d\n" result
    | _ -> Printf.printf "Unexpected result type\n"
  )
  | None -> Printf.printf "Program cannot be typed\n"
  | _ -> Printf.printf "Type error\n"

let test_program_2 () =
  Printf.printf "Program 2:\t";
  (* letfun fact x:int = if x < 2 then 1 else x * fact (x - 1) in fact 5 *)
  let program = Letfun (
    "fact", 
    "x",
    TFun (TInt, TInt),
    If (
      Op (Var "x", Less, IntLit 2),
      IntLit 1,
      Op (Var "x", Mul, App (Var "fact", Op (Var "x", Sub, IntLit 1)))
    ),
    App (Var "fact", IntLit 5)
  ) in
  match type_of [] program with
  | Some TInt -> (
    Printf.printf "Program is well-typed: int\t";
    match eval [] program with
    | IntVal result -> Printf.printf "Result: %d\n" result
    | _ -> Printf.printf "Unexpected result type\n"
  )
  | None -> Printf.printf "Program cannot be typed\n"
  | _ -> Printf.printf "Type error\n"

let test_program_3 () =
  Printf.printf "Program 3:\t";
  (* let f = (let a = 1 in (fun y:int => y + a)) in (let a = 2 in f 4) *)
  let program = Let (
    "f", 
    Let (
      "a",
      IntLit 1,
      Fun ("y", TInt, Op (Var "y", Add, Var "a"))
    ),
    Let (
      "a",
      IntLit 2,
      App (Var "f", IntLit 4)
    )
  ) in
  match type_of [] program with
  | Some TInt -> (
    Printf.printf "Program is well-typed: int\t";
    match eval [] program with
    | IntVal result -> Printf.printf "Result: %d\n" result
    | _ -> Printf.printf "Unexpected result type\n"
  )
  | None -> Printf.printf "Program cannot be typed\n"
  | _ -> Printf.printf "Type error\n"

let test_program_4 () =
  Printf.printf "Program 4:\t";
  (* fun is_negative : (int -> bool) = if 1 < 2 then true else false *)
  let program = Fun (
    "is_negative", 
    TInt,
    If (
      Op (IntLit 1, Less, IntLit 2),
      BoolLit true,
      BoolLit false
    )
  ) in
  match type_of [] program with
  | Some (TFun (TInt, TBool)) -> Printf.printf "Program is well-typed: int -> bool\n"
  | None -> Printf.printf "Program cannot be typed\n"
  | _ -> Printf.printf "Type error\n"

let () =
  Printf.printf "MiniTyFun Main:\n";
  test_program_1 ();
  test_program_2 ();
  test_program_3 ();
  test_program_4 ();
