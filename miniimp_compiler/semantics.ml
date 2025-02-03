open Ast

(* Type for the run-time environment *)
type environment = (var * int) list  (* Association list from var to int *)

(* Lookup function: Find the value of a variable in the environment *)
let rec lookup (env: environment) (v: var) : int option =
  match env with
  | [] -> None (* If the variable is not found, return None *)
  | (x, value) :: rest -> if x = v then Some value else lookup rest v (* If the variable is found, return its value, otherwise, search recursively on the rest of the list *)

(* IMPORTANT NOTE: Why do we pass and return env in functions? and not use the global ref?
- The environment is updated and returned by the funs (like eval_com, extend, etc) because we follow
an immutable design pattern, ensuring that the environment is always explicitly passed and returned,
and no side effects occur.
- This approach helps avoid the problems associated with mutable global state, like many accesses at the same time,
and aligns with functional programming best practices. *)

(* Extend function: Add a new variable and its value to the environment *)
(* TODO: change name *)
let extend (env: environment) (v: var) (value: int) : environment =
  (v, value) :: env

(* Evaluate arithmetic expressions *)
let rec eval_aexp (env: environment) (a: aexp) : int =
  match a with
  (* For integers, return theur value directly *)
  | Num n -> n
  (* For variables, look up the value in the environment, if not found, raise an error *)
  | Var v -> (match lookup env v with
              | Some value -> value
              | None -> failwith ("Variable " ^ v ^ " not defined"))
  (* For the operations, evaluate the sub-expressions and apply the op to their results *)
  | Plus (a1, a2) -> (eval_aexp env a1) + (eval_aexp env a2)
  | Minus (a1, a2) -> (eval_aexp env a1) - (eval_aexp env a2)
  | Times (a1, a2) -> (eval_aexp env a1) * (eval_aexp env a2)

(* Evaluate boolean expressions *)
let rec eval_bexp (env: environment) (b: bexp) : bool =
  match b with
  | Bool b -> b  (* Return the boolean value directly *)
  | And (b1, b2) -> (eval_bexp env b1) && (eval_bexp env b2)
  | Not b -> not (eval_bexp env b)
  | Less (a1, a2) -> (eval_aexp env a1) < (eval_aexp env a2)

(* Evaluate commands *)
let rec eval_com (env: environment) (c: com) : environment =
  match c with
  | Skip -> env  (* Skip command does nothing *)
  | Assign (v, a) -> 
      (* Calculate the value of the arithmetic exp then assign it to the var in our env *)
      let value = eval_aexp env a in
      extend env v value
  | Seq (c1, c2) -> 
      (* Evaluate the first command, then pass the resulting environment to the second command *)
      let env_ = eval_com env c1 in
      eval_com env_ c2 
  | If (b, c1, c2) -> 
      (* Evaluate the bool expr, if its true, execute c1, else execute c2 *)
      if eval_bexp env b then
        eval_com env c1
      else
        eval_com env c2 
  | While (b, c) -> 
      (* TODO: why we don't directly use ocaml while here?
      ex: while eval_bool_expr env b do eval_command env c *)
      if eval_bexp env b then 
        let env_ = eval_com env c in
        eval_com env_ (While (b, c))  (* Loop while b is true *)
      else env  (* Exit the loop when b is false *)
  | BQuestion b -> 
    (* Just evaluate the boolean expression and do nothing else
    'ignore' is used to discard the result of 'eval_bexp env b' because we don't need the boolean value. 
    It prevents a compiler warning about an unused result and ensures the environment 'env' remains unchanged.
    'ignore' is a built-in OCaml function that returns '()' after discarding any value.
    *)
    ignore (eval_bexp env b); 
    env

let eval_prg (env: environment) (p: program) (input_value: int) : environment =
  match p with
  | Main (x, _, c) -> (
      (* Extend the environment with the input value *)
      let env_ = extend env x input_value in
      eval_com env_ c
  )