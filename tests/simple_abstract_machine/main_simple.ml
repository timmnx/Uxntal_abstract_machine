module Colors = struct
  let _NORMAL, _RED, _GREEN, _BLUE, _YELLOW, _MAGENTA, _CYAN =
    ( "\x1B[0;0m"
    , "\x1B[1;31m"
    , "\x1B[1;32m"
    , "\x1B[1;33m"
    , "\x1B[1;34m"
    , "\x1B[1;35m"
    , "\x1B[1;36m" )
  ;;
end

(****************************************************************************)
(*                                                                          *)
(*    ######    ##      ##  ##      ##  ##########    ######    ##      ##  *)
(*    ######    ##      ##  ##      ##  ##########    ######    ##      ##  *)
(*  ##      ##  ##      ##  ##      ##      ##      ##      ##  ##      ##  *)
(*  ##      ##  ##      ##  ##      ##      ##      ##      ##  ##      ##  *)
(*  ##          ##      ##  ####    ##      ##      ##      ##  ##      ##  *)
(*  ##          ##      ##  ####    ##      ##      ##      ##  ##      ##  *)
(*    ######      ########  ##  ##  ##      ##      ##      ##    ######    *)
(*    ######      ########  ##  ##  ##      ##      ##      ##    ######    *)
(*          ##          ##  ##    ####      ##      ##########  ##      ##  *)
(*          ##          ##  ##    ####      ##      ##########  ##      ##  *)
(*  ##      ##  ##      ##  ##      ##      ##      ##      ##  ##      ##  *)
(*  ##      ##  ##      ##  ##      ##      ##      ##      ##  ##      ##  *)
(*    ######      ######    ##      ##      ##      ##      ##  ##      ##  *)
(*    ######      ######    ##      ##      ##      ##      ##  ##      ##  *)
(*                                                                          *)
(****************************************************************************)

module Syntax = struct
  type t =
    | Int of int
    | Add of t * t
    | Sub of t * t
    | Mul of t * t

  let rec to_string = function
    | Int i -> "Int " ^ string_of_int i
    | Add (s1, s2) -> "Add(" ^ to_string s1 ^ ", " ^ to_string s2 ^ ")"
    | Sub (s1, s2) -> "Sub(" ^ to_string s1 ^ ", " ^ to_string s2 ^ ")"
    | Mul (s1, s2) -> "Mul(" ^ to_string s1 ^ ", " ^ to_string s2 ^ ")"
  ;;

  let rec to_pretty_string = function
    | Int i -> string_of_int i
    | Add (s1, s2) -> to_pretty_string s1 ^ " + " ^ to_pretty_string s2
    | Sub (s1, s2) -> to_string s1 ^ " - " ^ to_string s2
    | Mul (s1, s2) ->
      (match s1, to_pretty_string s1 with
       | Int _, str -> str
       | _, str -> "(" ^ str ^ ")")
      ^ " * "
      ^
        (match s2, to_pretty_string s2 with
        | Int _, str -> str
        | _, str -> "(" ^ str ^ ")")
  ;;
end

(****************************************************************************************************************)
(*                                                                                                              *)
(*  ##########  ##      ##    ######    ##          ##      ##    ######    ##########    ######    ########    *)
(*  ##########  ##      ##    ######    ##          ##      ##    ######    ##########    ######    ########    *)
(*  ##          ##      ##  ##      ##  ##          ##      ##  ##      ##      ##      ##      ##  ##      ##  *)
(*  ##          ##      ##  ##      ##  ##          ##      ##  ##      ##      ##      ##      ##  ##      ##  *)
(*  ##          ##      ##  ##      ##  ##          ##      ##  ##      ##      ##      ##      ##  ##      ##  *)
(*  ##          ##      ##  ##      ##  ##          ##      ##  ##      ##      ##      ##      ##  ##      ##  *)
(*  ########    ##      ##  ##      ##  ##          ##      ##  ##      ##      ##      ##      ##  ########    *)
(*  ########    ##      ##  ##      ##  ##          ##      ##  ##      ##      ##      ##      ##  ########    *)
(*  ##          ##      ##  ##########  ##          ##      ##  ##########      ##      ##      ##  ##      ##  *)
(*  ##          ##      ##  ##########  ##          ##      ##  ##########      ##      ##      ##  ##      ##  *)
(*  ##            ##  ##    ##      ##  ##          ##      ##  ##      ##      ##      ##      ##  ##      ##  *)
(*  ##            ##  ##    ##      ##  ##          ##      ##  ##      ##      ##      ##      ##  ##      ##  *)
(*  ##########      ##      ##      ##  ##########    ######    ##      ##      ##        ######    ##      ##  *)
(*  ##########      ##      ##      ##  ##########    ######    ##      ##      ##        ######    ##      ##  *)
(*                                                                                                              *)
(****************************************************************************************************************)

module Evaluators = struct
  open Syntax

  let rec eval s =
    match s with
    | Int i -> i
    | Add (s1, s2) ->
      let i1 = eval s1 in
      let i2 = eval s2 in
      i1 + i2
    | Sub (s1, s2) ->
      let i1 = eval s1 in
      let i2 = eval s2 in
      i1 - i2
    | Mul (s1, s2) ->
      let i1 = eval s1 in
      let i2 = eval s2 in
      i1 * i2
  ;;

  let rec eval_cps s k =
    match s with
    | Int i -> k i
    | Add (s1, s2) -> eval_cps s1 (fun i1 -> eval_cps s2 (fun i2 -> k (i1 + i2)))
    | Sub (s1, s2) -> eval_cps s1 (fun i1 -> eval_cps s2 (fun i2 -> k (i1 - i2)))
    | Mul (s1, s2) -> eval_cps s1 (fun i1 -> eval_cps s2 (fun i2 -> k (i1 * i2)))
  ;;
end

(****************************************************************************************************)
(*                                                                                                  *)
(*    ######    ########      ######    ##########  ########      ######      ######    ##########  *)
(*    ######    ########      ######    ##########  ########      ######      ######    ##########  *)
(*  ##      ##  ##      ##  ##      ##      ##      ##      ##  ##      ##  ##      ##      ##      *)
(*  ##      ##  ##      ##  ##      ##      ##      ##      ##  ##      ##  ##      ##      ##      *)
(*  ##      ##  ##      ##  ##              ##      ##      ##  ##      ##  ##              ##      *)
(*  ##      ##  ##      ##  ##              ##      ##      ##  ##      ##  ##              ##      *)
(*  ##      ##  ########      ######        ##      ########    ##      ##  ##              ##      *)
(*  ##      ##  ########      ######        ##      ########    ##      ##  ##              ##      *)
(*  ##########  ##      ##          ##      ##      ##      ##  ##########  ##              ##      *)
(*  ##########  ##      ##          ##      ##      ##      ##  ##########  ##              ##      *)
(*  ##      ##  ##      ##  ##      ##      ##      ##      ##  ##      ##  ##      ##      ##      *)
(*  ##      ##  ##      ##  ##      ##      ##      ##      ##  ##      ##  ##      ##      ##      *)
(*  ##      ##  ########      ######        ##      ##      ##  ##      ##    ######        ##      *)
(*  ##      ##  ########      ######        ##      ##      ##  ##      ##    ######        ##      *)
(*                                                                                                  *)
(*                                                                                                  *)
(*  ##      ##    ######      ######    ##      ##      ##      ##      ##  ##########              *)
(*  ##      ##    ######      ######    ##      ##      ##      ##      ##  ##########              *)
(*  ####  ####  ##      ##  ##      ##  ##      ##      ##      ##      ##  ##                      *)
(*  ####  ####  ##      ##  ##      ##  ##      ##      ##      ##      ##  ##                      *)
(*  ##  ##  ##  ##      ##  ##          ##      ##      ##      ####    ##  ##                      *)
(*  ##  ##  ##  ##      ##  ##          ##      ##      ##      ####    ##  ##                      *)
(*  ##      ##  ##      ##  ##          ##########      ##      ##  ##  ##  ########                *)
(*  ##      ##  ##      ##  ##          ##########      ##      ##  ##  ##  ########                *)
(*  ##      ##  ##########  ##          ##      ##      ##      ##    ####  ##                      *)
(*  ##      ##  ##########  ##          ##      ##      ##      ##    ####  ##                      *)
(*  ##      ##  ##      ##  ##      ##  ##      ##      ##      ##      ##  ##                      *)
(*  ##      ##  ##      ##  ##      ##  ##      ##      ##      ##      ##  ##                      *)
(*  ##      ##  ##      ##    ######    ##      ##      ##      ##      ##  ##########              *)
(*  ##      ##  ##      ##    ######    ##      ##      ##      ##      ##  ##########              *)
(*                                                                                                  *)
(****************************************************************************************************)

module AbstractMachine = struct
  open Syntax

  type t = Syntax.t
  (* type env = string -> int *)

  type cont =
    | Id
    | Fn_add of int * cont
    | Ar_add of t * cont
    | Fn_sub of int * cont
    | Ar_sub of t * cont
    | Fn_mul of int * cont
    | Ar_mul of t * cont

  let rec cont_to_string = function
    | Id -> Colors._NORMAL ^ "id"
    | Fn_add (i, k) -> Colors._RED ^ "Fn+(" ^ string_of_int i ^ ") " ^ Colors._BLUE ^ ":: " ^ cont_to_string k
    | Fn_sub (i, k) -> Colors._RED ^ "Fn-(" ^ string_of_int i ^ ") " ^ Colors._BLUE ^ ":: " ^ cont_to_string k
    | Fn_mul (i, k) -> Colors._RED ^ "Fn*(" ^ string_of_int i ^ ") " ^ Colors._BLUE ^ ":: " ^ cont_to_string k
    | Ar_add (s, k) -> Colors._GREEN ^ "Ar+(" ^ to_string s ^ ") " ^ Colors._BLUE ^ ":: " ^ cont_to_string k
    | Ar_sub (s, k) -> Colors._GREEN ^ "Ar-(" ^ to_string s ^ ") " ^ Colors._BLUE ^ ":: " ^ cont_to_string k
    | Ar_mul (s, k) -> Colors._GREEN ^ "Ar*(" ^ to_string s ^ ") " ^ Colors._BLUE ^ ":: " ^ cont_to_string k
  ;;

  let rec eval (s, k) show =
    if show
    then
      print_endline ("< " ^ to_string s ^ " || " ^ cont_to_string k ^ " >e");
    match s with
    | Int i -> apply (k, i) show
    | Add (s1, s2) -> eval (s1, Ar_add (s2, k)) show
    | Sub (s1, s2) -> eval (s1, Ar_sub (s2, k)) show
    | Mul (s1, s2) -> eval (s1, Ar_mul (s2, k)) show

  and apply (k, i) show =
    if show
    then
      print_endline
        ("< " ^ cont_to_string k ^ " || " ^ string_of_int i ^ " >a");
    match k with
    | Id -> i
    | Ar_add (s2, k') -> eval (s2, Fn_add (i, k')) show
    | Fn_add (i1, k') -> eval (Int (i1 + i), k') show
    | Ar_sub (s2, k') -> eval (s2, Fn_sub (i, k')) show
    | Fn_sub (i1, k') -> eval (Int (i1 - i), k') show
    | Ar_mul (s2, k') -> eval (s2, Fn_mul (i, k')) show
    | Fn_mul (i1, k') -> eval (Int (i1 * i), k') show
  ;;
end

(**************************************************)
(*                                                *)
(*  ##      ##    ######      ##      ##      ##  *)
(*  ##      ##    ######      ##      ##      ##  *)
(*  ####  ####  ##      ##    ##      ##      ##  *)
(*  ####  ####  ##      ##    ##      ##      ##  *)
(*  ##  ##  ##  ##      ##    ##      ####    ##  *)
(*  ##  ##  ##  ##      ##    ##      ####    ##  *)
(*  ##      ##  ##      ##    ##      ##  ##  ##  *)
(*  ##      ##  ##      ##    ##      ##  ##  ##  *)
(*  ##      ##  ##########    ##      ##    ####  *)
(*  ##      ##  ##########    ##      ##    ####  *)
(*  ##      ##  ##      ##    ##      ##      ##  *)
(*  ##      ##  ##      ##    ##      ##      ##  *)
(*  ##      ##  ##      ##    ##      ##      ##  *)
(*  ##      ##  ##      ##    ##      ##      ##  *)
(*                                                *)
(**************************************************)

let main () =
  let exprs =
    [| Syntax.Add (Int 1, Int 2)
     ; Syntax.Sub (Int 1, Int 2)
     ; Syntax.Add (Int 1, Mul (Int 2, Int 3))
     ; Syntax.Add (Mul (Int 3, Int (-2)), Mul (Int 7, Int 3))
    |]
  in
  for i = 0 to Array.length exprs - 1 do
    let s = Syntax.to_pretty_string exprs.(i) in
    let blank = String.init (String.length s) (fun _ -> ' ') in
    print_endline (s ^ " = " ^ string_of_int @@ Evaluators.eval exprs.(i));
    print_endline
      (blank ^ " = " ^ string_of_int @@ Evaluators.eval_cps exprs.(i) (fun x -> x));
    print_endline
      (blank
       ^ " = "
       ^ string_of_int
       @@ AbstractMachine.eval (exprs.(i), AbstractMachine.Id) false);
    print_newline ()
  done;
  let _ = AbstractMachine.eval (exprs.(3), AbstractMachine.Id) true in
  print_newline ();
  ()
;;

let () = main ()
