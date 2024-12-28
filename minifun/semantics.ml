open Ast

(* Environment: List of variable/function bindings *)
type 'a env = (ident * 'a) list

(* Look up a variable in the environment *)
let lookup env x = 
  try List.assoc x env with Not_found -> failwith ("Unbound variable: " ^ x)

(* Update the environment by adding a new binding (var or fun) *)
let extend env x v = (x, v) :: env

(* Types for values *)
type value = 
  | IntVal of int           (* Integer value *)
  | BoolVal of bool         (* Boolean value *)
  | Closure of ident * expr * value env  (* Closure: (x, t, env) *)
  | RecClosure of ident * ident * expr * value env  (* Recursive closure: (f, x, t, env) *)
(* PS: f = fun name; x = param; t = fun body; env = enviorment *)

(* 3. Translate the Deduction System into Functions *)

(* Translate the deduction system rules (from the semantic pdf slides) into functions that perform
the evaluation of MiniFun terms, such as literals, variables, functions, let bindings, and operations. *)

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

  (* Function: create closures. fun x => t *)
  | Fun (x, t) -> Closure (x, t, env)

  (* Let bindings: evaluate the bound term and then evaluate the body with the updated environment. *)
  | Let (x, t1, t2) ->
    (* let x = t1 in t2 *)
    let v1 = eval env t1 in
    let env_ = extend env x v1 in (* update the value of x with the value of t1 in the env*)
    eval env_ t2 (* use the updated env to evaluate t2 *)

  (* Recursive functions: create recursive closures and evaluate the function body in the extended environment. *)
  | Letfun (f, x, t1, t2) ->
    (* letfun f x = t1 in t2 *)
    (* TODO: make sure using "rec" here is correct, otherwise remove it *)
    let closure = RecClosure (f, x, t1, env) in
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