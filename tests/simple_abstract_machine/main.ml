module Syntax = struct
  type t =
    | Error
    | Int of int
    | Add of t * t
    | Sub of t * t
    | Mul of t * t
    | Div of t * t
    | Mod of t * t

  let rec to_string = function
    | Error -> "Error"
    | Int i -> string_of_int i
    | Add (s1, s2) -> to_string s1 ^ " + " ^ to_string s2
    | Sub (s1, s2) -> to_string s1 ^ " - " ^ to_string s2
    | Mul (s1, s2) ->
      (match s1, to_string s1 with
       | Int _, str -> str
       | _, str -> "(" ^ str ^ ")")
      ^ " * "
      ^
        (match s2, to_string s2 with
        | Int _, str -> str
        | _, str -> "(" ^ str ^ ")")
    | Div (s1, s2) ->
      (match s1, to_string s1 with
       | Int _, str -> str
       | _, str -> "(" ^ str ^ ")")
      ^ " / "
      ^
        (match s2, to_string s2 with
        | Int _, str -> str
        | _, str -> "(" ^ str ^ ")")
    | Mod (s1, s2) ->
      (match s1, to_string s1 with
       | Int _, str -> str
       | _, str -> "(" ^ str ^ ")")
      ^ " % "
      ^
        (match s2, to_string s2 with
        | Int _, str -> str
        | _, str -> "(" ^ str ^ ")")
  ;;

  let simplify = function
    | Add (Int i1, Int i2) -> Int (i1 + i2)
    | Sub (Int i1, Int i2) -> Int (i1 - i2)
    | Mul (Int i1, Int i2) -> Int (i1 * i2)
    | Div (Int i1, Int i2) when i2 <> 0 -> Int (i1 / i2)
    | Mod (Int i1, Int i2) when i2 <> 0 -> Int (i1 mod i2)
    | _ -> Error
  ;;
end

module Evaluators = struct
  open Syntax

  let rec eval s =
    match s with
    | Error | Int _ -> s
    | Add (s1, s2) ->
      let s1' = eval s1 in
      let s2' = eval s2 in
      simplify (Add (s1', s2'))
    | Sub (s1, s2) ->
      let s1' = eval s1 in
      let s2' = eval s2 in
      simplify (Sub (s1', s2'))
    | Mul (s1, s2) ->
      let s1' = eval s1 in
      let s2' = eval s2 in
      simplify (Mul (s1', s2'))
    | Div (s1, s2) ->
      let s1' = eval s1 in
      let s2' = eval s2 in
      simplify (Div (s1', s2'))
    | Mod (s1, s2) ->
      let s1' = eval s1 in
      let s2' = eval s2 in
      simplify (Mod (s1', s2'))
  ;;

  let rec eval_cps s k =
    match s with
    | Error | Int _ -> k s
    | Add (s1, s2) ->
      eval_cps s1 (fun s1' -> eval_cps s2 (fun s2' -> k @@ simplify (Add (s1', s2'))))
    | Sub (s1, s2) ->
      eval_cps s1 (fun s1' -> eval_cps s2 (fun s2' -> k @@ simplify (Sub (s1', s2'))))
    | Mul (s1, s2) ->
      eval_cps s1 (fun s1' -> eval_cps s2 (fun s2' -> k @@ simplify (Mul (s1', s2'))))
    | Div (s1, s2) ->
      eval_cps s1 (fun s1' -> eval_cps s2 (fun s2' -> k @@ simplify (Div (s1', s2'))))
    | Mod (s1, s2) ->
      eval_cps s1 (fun s1' -> eval_cps s2 (fun s2' -> k @@ simplify (Mod (s1', s2'))))
  ;;
end

module AbstractMachine = struct
  open Syntax

  type t = Syntax.t
  type env = string -> int option

  type cont =
    | Id
    | Fn_add of t * cont
    | Fn_sub of t * cont
    | Fn_mul of t * cont
    | Fn_div of t * cont
    | Fn_mod of t * cont
    (* | Ar_add of t * cont
    | Ar_sub of t * cont
    | Ar_mul of t * cont
    | Ar_div of t * cont
    | Ar_mod of t * cont *)
  ;;

  (* let rec eval = function
    | Error, k -> apply (k, Error)
    | Int i, k -> apply (k, Int i)
    | Add (s1, s2), k -> eval (s2, Fn_add(s1, k))
    | Sub (s1, s2), k -> eval (s2, Fn_sub(s1, k))
    | Mul (s1, s2), k -> eval (s2, Fn_mul(s1, k))
    | Div (s1, s2), k -> eval (s2, Fn_div(s1, k))
    | Mod (s1, s2), k -> eval (s2, Fn_mod(s1, k))
  and apply = function
    | Id, s -> s
    | Fn_add(s1, k), v -> eval (s1, )
    | _ -> failwith "ToDo"
  ;; *)
end

let main () =
  let exprs =
    [| Syntax.Add (Int 1, Int 2)
     ; Syntax.Sub (Int 1, Int 2)
     ; Syntax.Mul (Int 3, Int 2)
     ; Syntax.Div (Int 15, Int 4)
     ; Syntax.Mod (Int 15, Int 4)
     ; Syntax.Div (Int 10, Add (Int 2, Int (-2)))
    |]
  in
  for i = 0 to Array.length exprs do
    let s = Syntax.to_string exprs.(i) in
    let blank = String.init (String.length s) (fun _ -> ' ') in
    print_endline (s ^ " = " ^ Syntax.to_string @@ Evaluators.eval exprs.(i));
    print_endline
      (blank ^ " = " ^ Syntax.to_string @@ Evaluators.eval_cps exprs.(i) (fun x -> x));
    print_newline ()
  done;
  ()
;;

let () = main ()
