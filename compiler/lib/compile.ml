open Uxn

let string_pp fmt = Format.fprintf fmt "%s"
let list_pp = IrPrinting.list_pp

let compile_types fmt =
  List.iter (fun (s, Ir.Constructors cl) ->
    Format.fprintf fmt "( %s )@." s;
    List.iteri
      (fun i (name, args) -> Format.fprintf fmt "%%%s { #%02X%02X }@." name (i + 1) args)
      cl)
;;

let rec var_pp fmt = function
  | Var x -> string_pp fmt x
  | Read (v, i) -> Format.fprintf fmt "%a[%d]" var_pp v i
;;

let compile_stack fmt (Stack (ws, rs)) =
  Format.fprintf
    fmt
    "( ws: %a --- rs: %a )"
    (fun fmt' l -> List.iter (Format.fprintf fmt' "%a " var_pp) l)
    ws
    (fun fmt' l -> List.iter (Format.fprintf fmt' "%a " var_pp) l)
    rs
;;

(* let rec eq v1 v2 =
  match v1, v2 with
  | Var x1, Var x2 -> x1 = x2
  | Read (v1', i1), Read (v2', i2) when i1 = i2 -> eq v1' v2'
  | _ -> false
;; *)

let get_var_in_stack ?(dup=true) (x : string) (Stack (ws, _)) fmt_g =
  let rec aux first_round fmt l =
    match l with
    | [] -> failwith "Stack empty"
    | Var x' :: _ when x = x' && first_round -> ()
    | Var x' :: _ when x = x' -> Format.fprintf fmt "DUP2"
    | _ :: t -> Format.fprintf fmt "STH2 %a STH2r SWP2" (aux false) t
  in
  aux (not dup) fmt_g ws
;;

(* let rec clean_stack fmt = function
| Stack([], _), Stack([], _) -> ()
| Stack([], _), _ -> failwith "Where is the bug ??"
|  *)

let keep_fst_stack fmt (s : stack) =
  match s with
  | Stack([], _) -> failwith "Can't keep fist element of the stack, it is empty"
  | Stack(_::t, _) -> Format.fprintf fmt "STH2 %sSTH2r@;" (List.fold_left (fun acc _ -> "POP2 " ^ acc) "" t) 

(* let mem_stack (v : var) (Stack (ws, _)) = List.exists (eq v) ws *)

let rec compile_var ?(dup=true) s fmt v =
  match v with
  | Var x ->
    let (Stack (ws, rs)) = s in
    Format.fprintf
      fmt
      "%a   %a@;"
      (fun fmt x -> get_var_in_stack ~dup x s fmt)
      x
      compile_stack
      (Stack (v :: ws, rs))
  | Read (v', i) ->
    Format.fprintf fmt "%a #%04X ADD2 LDA2" (compile_var s) v' (2 * (i + 1))
;;

let rec compile_exp (t : Ir.types) fmt = function
  | Value (s_in, v, s_out) ->
    Format.fprintf
      fmt
      "%a   %a %a@;"
      (compile_var s_in)
      v
      compile_stack
      s_in
      compile_stack
      s_out
  | Obj (s_in, name, args, s_out) ->
    Format.fprintf
      fmt
      "%a%s ;push_obj JSR2   %a %a@;"
      (fun f l -> l |> List.rev |> List.iter (Format.fprintf f "%a " (compile_exp t)))
      args
      name
      compile_stack
      s_in
      compile_stack
      s_out
  | UxnCall (s_in, f, args, s_out) ->
    Format.fprintf
      fmt
      "%a %s   %a %a@;"
      (fun f l -> l |> List.iter (Format.fprintf f "%a " (compile_exp t)))
      args
      f
      compile_stack
      s_in
      compile_stack
      s_out
  | Call (s_in, f, args, s_out) ->
    Format.fprintf
      fmt
      "%a ;%s JSR2   %a %a@;"
      (fun f l -> l |> List.iter (Format.fprintf f "%a " (compile_exp t)))
      args
      f
      compile_stack
      s_in
      compile_stack
      s_out
;;

let rec compile_stmt (t : Ir.types) fmt = function
  | Ret (s_in, e, s_out) ->
    Format.fprintf
      fmt
      "%a %a JMP2r %a %a"
      (compile_exp t)
      e
      keep_fst_stack (UxnFromIr.exp_stacks e |> snd)
      compile_stack
      s_in
      compile_stack
      s_out
  | Case (s_in, v, stack_tag_f_stack) ->
    Format.fprintf fmt "%a   %a@;" (compile_var ~dup:false s_in) v compile_stack s_in ;
    List.iter
      (fun (tag, f, s_out) ->
         Format.fprintf
           fmt
           "LDA2k %s EQU2 ;%s JCN2  %a@;" (* NE PAS OUBLIER LE 2 dans JCN2*)
           tag
           f
           compile_stack
           s_out)
      stack_tag_f_stack;
    Format.fprintf fmt "#0000 JSR2r"
  | _ -> failwith "ToDo"
;;

let rec compile_decl (t : Ir.types) fmt (Fun (name, s_in, b, s_out)) =
  Format.fprintf
    fmt
    "%@%s   %a %a@;  @[<v>%a@]"
    name
    compile_stack
    s_in
    compile_stack
    s_out
    (compile_stmt t)
    b
;;

let compile_header fmt ((t, _) : program) =
  compile_types fmt t
;;

let compile fmt ((t, dl) : program) =
  List.iter (fun d -> Format.fprintf fmt "@.%a@." (compile_decl t) d) dl
;;
