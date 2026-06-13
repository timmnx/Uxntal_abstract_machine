open Ir

let string_pp fmt = Format.fprintf fmt "%s"

let list_pp ?(breakLine : bool = false) pp fmt l =
  Format.fprintf fmt (if breakLine then "@[<v>" else "@[<h>");
  List.iter (fun x -> Format.fprintf fmt "%a @;:: " pp x) l;
  Format.fprintf fmt "[]@]"
;;

(* let list_pp ?(breakLine : bool = false) pp fmt a =
  Format.fprintf fmt (if breakLine then "[|@[<v>" else "[|@[<h>");
  Array.iteri
    (fun i x -> Format.fprintf fmt (if i = 0 then "{%d}%a" else "@;; {%d}%a") i pp x)
    a;
  Format.fprintf fmt "|]@]"
;; *)

let constructors_pp fmt (Constructors t) =
  Format.fprintf
    fmt
    "@[<h>Constructors(%a)@]"
    (list_pp (fun f (s, i) -> Format.fprintf f "%s(%d)" s i))
    t
;;

let types_pp fmt =
  Format.fprintf
    fmt
    "types(%a)"
    (list_pp ~breakLine:true (fun f (t, c) ->
       Format.fprintf f "%s ::= %a" t constructors_pp c))
;;

let rec exp_pp fmt = function
  | Var x -> Format.fprintf fmt "Var %s" x
  | Obj (o, l) -> Format.fprintf fmt "Obj(%s, %a)" o (list_pp exp_pp) l
  | Call (f, l) -> Format.fprintf fmt "Call(%s, %a)" f (list_pp exp_pp) l
;;

let pat_pp fmt = function
  | Pattern (o, l) -> Format.fprintf fmt "Pattern(%s, %a)" o (list_pp string_pp) l
  | Default -> Format.fprintf fmt "Default"
;;

let rec body_pp fmt = function
  | Let (x, e, b) ->
    Format.fprintf fmt "@[<hv>Let (%s , %a,@; %a@;)@]" x exp_pp e body_pp b
  | Ret e -> Format.fprintf fmt "@[<h>Ret %a@]" exp_pp e
  | Case (x, pl) ->
    Format.fprintf
      fmt
      "@[<v>Case (%s,@;    %a)@]"
      x
      (list_pp ~breakLine:true (fun f (p, b) ->
         Format.fprintf f "%a ->@;   %a" pat_pp p body_pp b))
      pl
;;

let fun_pp fmt (Fun (name, args, b, (in_type, out_type))) =
  Format.fprintf
    fmt
    "@[<v>%s -> %s@;Fun (%s, %a, @;  %a@;)@]"
    in_type
    out_type
    name
    (list_pp string_pp)
    args
    body_pp
    b
;;

let program_pp fmt ((t, p) : program) =
  Format.fprintf fmt "%a@.@.%a" types_pp t (list_pp ~breakLine:true fun_pp) p
;;
