open Ir

let rec typeName_to_string = function
  | Ast.Unit -> "Unit"
  | Ast.SimpleType s -> s
  | Ast.PrimeType c -> Format.sprintf "%c" c
  | Ast.GenericType (t, s) -> Format.sprintf "<%s>%s" (typeName_to_string t) s
  | Ast.Cross l ->
    List.fold_left
      (fun (i, acc) t ->
         i + 1, Format.sprintf "%s%s" (if i = 0 then "" else "#") (typeName_to_string t))
      (-1, "") (* MAGIE NOIRE... pourquoi -1 ??? *)
      l
    |> snd
;;

let translate_to_types =
  List.map (fun (Ast.Type (t, cl)) ->
    ( typeName_to_string t
    , Constructors
        (List.map (fun (Ast.Constructor (name, tl)) -> name, List.length tl) cl) ))
;;

let lookup (sub : (string, string) Hashtbl.t) (x : string) =
  match Hashtbl.find_opt sub x with
  | None -> x
  | Some v -> v
;;

let rec translate_to_exp (sub : (string, string) Hashtbl.t) = function
  | Ast.Var x -> Var (lookup sub x)
  | Ast.Tuple l -> Obj ("tuple", List.map (translate_to_exp sub) l)
  | Ast.Object (o, l) -> Obj (o, List.map (translate_to_exp sub) l)
  | Ast.FunCall (f, l) -> Call (f, List.map (translate_to_exp sub) l)
;;

let matchExpr_to_string
      (i : int)
      (match_var_name : string)
      (sub : (string, string) Hashtbl.t)
  = function
  | Ast.MatchVar x -> begin
    let v = Format.sprintf "%s@%d" (lookup sub match_var_name) i in
    Hashtbl.replace sub x v;
    v
  end
  | _ -> failwith "Should be a Variable"
;;

let rec translate_to_pat (match_var_name : string) (sub : (string, string) Hashtbl.t)
  = function
  | Ast.MatchVar _ -> Default, sub
  | Ast.MatchTuple l -> begin
    let sub' = Hashtbl.copy sub in
    ( Pattern
        ("tuple", List.mapi (fun i me -> matchExpr_to_string i match_var_name sub' me) l)
    , sub' )
  end
  | Ast.MatchObject (o, l) -> begin
    let sub' = Hashtbl.copy sub in
    ( Pattern (o, List.mapi (fun i me -> matchExpr_to_string i match_var_name sub' me) l)
    , sub' )
  end
;;

let rec translate_to_body (sub : (string, string) Hashtbl.t) = function
  | Ast.Let (x, ex, s) -> Let (x, translate_to_exp sub ex, translate_to_body sub s)
  | Ast.Ret ex -> Ret (translate_to_exp sub ex)
  | Ast.Match (x, l) ->
    Case
      ( lookup sub x
      , List.map
          (fun (me, s) ->
             let translated_pat, sub' = translate_to_pat x sub me in
             translated_pat, translate_to_body sub' s)
          l )
;;

let rec translate_to_decl = function
  | Ast.Fun f ->
    Fun
      ( f.name
      , List.map fst f.argumentsTyped
      , translate_to_body
          (Hashtbl.create 0)
          f.body (* , translate_to_body [var_names] f.body *)
      , ( Cross (List.map snd f.argumentsTyped) |> typeName_to_string
        , f.return |> typeName_to_string ) )
;;

let translate_to_ir (m : Ast.machine) : program =
  let x = Ast.delta_get_argument m.d in
  let sub = Hashtbl.create 0 in
  let l =
    [ Fun
        ( "step"
        , [ x ]
        , Case
            ( x
            , List.map
                (fun (me, s) ->
                   let translated_pat, sub' = translate_to_pat x sub me in
                   translated_pat, translate_to_body sub' s)
                (Ast.delta_get_rules m.d) )
        , m.d |> Ast.delta_get_workingType |> typeName_to_string |> fun x -> x, x )
    ]
  in
  translate_to_types m.t, List.fold_left (fun acc f -> translate_to_decl f :: acc) l m.f
;;
