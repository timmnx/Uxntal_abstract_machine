open Ast

let rec typeName_pp (fmt : Format.formatter) = function
  | Unit -> Format.fprintf fmt "unit"
  | SimpleType t -> Format.fprintf fmt "%s" t
  | PrimeType a -> Format.fprintf fmt "'%c" a
  | GenericType (gen, t) -> Format.fprintf fmt "%a %s" typeName_pp gen t
  | Cross [] -> failwith "typeName_pp : empty cross"
  | Cross (h :: tl) ->
    typeName_pp fmt h;
    List.iter (fun t -> Format.fprintf fmt " * %a" typeName_pp t) tl
;;

let valName_pp (fmt : Format.formatter) = function
  | ValueStatic (s, t) -> Format.fprintf fmt "val %s : %a" s typeName_pp t
  | ValueFun (f, t1, t2) ->
    Format.fprintf fmt "val %s : (%a) -> %a " f typeName_pp t1 typeName_pp t2
;;

let externals_pp (fmt : Format.formatter) { externTypes; externValues } =
  List.iter (fun t -> Format.fprintf fmt "type %a@." typeName_pp t) externTypes;
  List.iter (fun t -> Format.fprintf fmt "%a@." valName_pp t) externValues
;;

let constructor_pp (fmt : Format.formatter) = function
  | Constructor (n, []) -> Format.fprintf fmt "%s" n
  | Constructor (n, h :: tl) ->
    Format.fprintf fmt "%s of %a" n typeName_pp (Cross (h :: tl))
;;

let typeDecl_pp (fmt : Format.formatter) = function
  | Type (_, []) -> failwith "typeDecl_pp : empty type declaration"
  | Type (t, cs) ->
    Format.fprintf fmt "@[<v>type %a =" typeName_pp t;
    List.iter (fun c -> Format.fprintf fmt "@;  | %a" constructor_pp c) cs;
    Format.fprintf fmt "@]@."
;;

let types_pp (fmt : Format.formatter) =
  List.iter (fun t -> Format.fprintf fmt "%a@." typeDecl_pp t)
;;

let rec expr_pp (fmt : Format.formatter) = function
  | Var x -> Format.fprintf fmt "%s" x
  | Tuple [] | Tuple [ _ ] -> failwith "expr_pp : tuple containing less than 2 elements"
  | Tuple (h :: t) ->
    Format.fprintf fmt "(%a" expr_pp h;
    List.iter (fun x -> Format.fprintf fmt ", %a" expr_pp x) t;
    Format.fprintf fmt ")"
  | Object (c, []) -> Format.fprintf fmt "%s" c
  | Object (c, [ e ]) -> Format.fprintf fmt "%s %a" c expr_pp e
  | Object (c, t) -> Format.fprintf fmt "%s%a" c expr_pp (Tuple t)
  | FunCall (f, []) -> Format.fprintf fmt "%s()" f
  | FunCall (f, [ arg ]) -> Format.fprintf fmt "%s(%a)" f expr_pp arg
  | FunCall (f, args) -> Format.fprintf fmt "%s%a" f expr_pp (Tuple args)
;;

let rec matchExpr_pp (fmt : Format.formatter) = function
  | MatchVar x -> Format.fprintf fmt "%s" x
  | MatchTuple [] | MatchTuple [ _ ] ->
    failwith "expr_pp : tuple containing less than 2 elements"
  | MatchTuple (h :: t) ->
    Format.fprintf fmt "(%a" matchExpr_pp h;
    List.iter (fun x -> Format.fprintf fmt ", %a" matchExpr_pp x) t;
    Format.fprintf fmt ")"
  | MatchObject (c, []) -> Format.fprintf fmt "%s" c
  | MatchObject (c, [ e ]) -> Format.fprintf fmt "%s %a" c matchExpr_pp e
  | MatchObject (c, t) -> Format.fprintf fmt "%s%a" c matchExpr_pp (MatchTuple t)
;;

(* let rec statement_pp ?(indent = 0) = function
  | Let (x, e, s) ->
    String.make indent '\t' ^ "let " ^ x ^ " = " ^ expr_pp e ^ " in\n" ^ statement_pp ~indent:indent s
  | Ret e -> String.make indent '\t' ^ expr_pp e ^ "\n"
  | Match (x, m) ->
    let sindent = String.make indent '\t' in
    List.fold_left
      (fun acc (me, s) ->
         acc ^ sindent ^ "| " ^ matchExpr_pp me ^ " -> begin\n" ^ statement_pp ~indent:(indent+1) s ^ sindent ^"  end\n")
      (sindent ^ "match " ^ x ^ " with\n")
      m
;; *)
let rec statement_pp (fmt : Format.formatter) = function
  | Let (x, e, s) -> Format.fprintf fmt "let %s = %a in %a" x expr_pp e statement_pp s
  | Ret e -> expr_pp fmt e
  | Match (x, m) ->
    Format.fprintf fmt "@[<v>match %s with" x;
    List.iter
      (fun (m, s) ->
         Format.fprintf
           fmt
           "@;| %a -> begin@;    @[<v>%a@]@;  end"
           matchExpr_pp
           m
           statement_pp
           s)
      m;
    Format.fprintf fmt "@]"
;;

(* let function_pp = function
  | Fun f ->
    "val "
    ^ f.name
    ^ (match f.argumentsTyped with
       | [] -> "("
       | (a, t) :: args ->
         List.fold_left
           (fun acc (arg, typ) -> acc ^ ", " ^ arg ^ ": " ^ typeName_pp typ)
           ("(" ^ a ^ ": " ^ typeName_pp t)
           args)
    ^ "): "
    ^ typeName_pp f.return
    ^ " =\n"
    ^ statement_pp f.body
;; *)
let function_pp (fmt : Format.formatter) (Fun f : funDecl) =
  Format.fprintf fmt "val %s (" f.name;
  List.iteri
    (fun i (arg, typ) ->
       Format.fprintf fmt "%s%s: %a" (if i = 0 then "" else ", ") arg typeName_pp typ)
    f.argumentsTyped;
  Format.fprintf fmt ") : %a = @.  @[<v>%a@]" typeName_pp f.return statement_pp f.body
;;

let functions_pp (fmt : Format.formatter) =
  List.iter (Format.fprintf fmt "%a@." function_pp)
;;

let delta_pp (fmt : Format.formatter) (Delta d : delta) =
  Format.fprintf
    fmt
    "val delta (%s: %a) : %a = @.@[<v>  match %s with"
    d.argument
    typeName_pp
    d.workingType
    typeName_pp
    d.workingType
    d.argument;
  List.iter
    (fun (m, s) ->
       Format.fprintf
         fmt
         "@;  | %a -> begin@;      @[<v>%a@]@;    end"
         matchExpr_pp
         m
         statement_pp
         s)
    d.rules;
  Format.fprintf fmt "@]"
;;

let print_machine (fmt : Format.formatter) (m : machine) : unit =
  Format.fprintf
    fmt
    "%a@.@.%a@.@.%a@.@.%a@."
    externals_pp
    m.e
    types_pp
    m.t
    functions_pp
    m.f
    delta_pp
    m.d
;;

(* let de_curryfy (m: machine): machine =
  let rec need_to_change : matchExpr -> bool = function
  | MatchObject(_, mel) -> List.exists (function MatchObject _ -> true | _ -> false) mel
  | _ -> false
  in
  if need_to_change m then begin

  end *)
