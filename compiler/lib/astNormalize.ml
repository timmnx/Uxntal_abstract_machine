open Ast

module Fresh = struct
  let i = ref 0

  let var () =
    incr i;
    "%v" ^ string_of_int !i
  ;;

  let reset () = i := 0
end

module Desugar = struct
  let rec desugar_statement (s : statement) : statement =
    match s with
    | Let (x, e, s') -> Let (x, e, desugar_statement s')
    | Ret _ -> s
    | Match (x, l) -> Match (x, List.map desugar_match l)

  and desugar_match ((me, s) : matchExpr * statement) : matchExpr * statement =
    match me with
    | MatchVar _ -> me, s
    | MatchTuple t -> begin
      let t' = ref [] in
      let s'' =
        List.fold_right
          (fun me' s' ->
             match me' with
             | MatchVar x -> begin
               t' := me' :: !t';
               s'
             end
             | _ -> begin
               let x = Fresh.var () in
               t' := MatchVar x :: !t';
               Match (x, [ me', s' ]) |> desugar_statement
             end)
          t
          s
      in
      MatchTuple !t', s''
    end
    | MatchObject (o, l) -> begin
      let l' = ref [] in
      let s'' =
        List.fold_right
          (fun me' s' ->
             match me' with
             | MatchVar x ->
               l' := me' :: !l';
               s'
             | _ ->
               let x = Fresh.var () in
               l' := MatchVar x :: !l';
               Match (x, [ me', s' ]) |> desugar_statement)
          l
          s
      in
      MatchObject (o, !l'), s''
    end
  ;;

  let desugar (m : machine) : machine =
    Fresh.reset ();
    { e = m.e
    ; t = m.t
    ; f =
        List.map
          (fun (Fun f') ->
             Fun
               { name = f'.name
               ; argumentsTyped = f'.argumentsTyped
               ; return = f'.return
               ; body = desugar_statement f'.body
               })
          m.f
    ; d =
        (match m.d with
         | Delta d' ->
           Delta
             { workingType = d'.workingType
             ; argument = d'.argument
             ; rules = List.map desugar_match d'.rules
             })
    }
  ;;
end

module Merge = struct
  let order_match_expr me1 me2 =
    match me1, me2 with
    | MatchVar x1, MatchVar x2 -> String.compare x1 x2
    | MatchVar _, _ -> 1
    | _, MatchVar _ -> -1
    | MatchObject (n1, _), MatchObject (n2, _) -> String.compare n1 n2
    | MatchTuple t1, MatchTuple t2 -> 0
    | _ -> failwith "Impossible ordering"
  ;;

  let rec substitute_expr (sigma : string -> string) = function
    | Var x -> Var (sigma x)
    | Tuple l -> Tuple (List.map (substitute_expr sigma) l)
    | Object (o, l) -> Object (o, List.map (substitute_expr sigma) l)
    | FunCall (f, l) -> FunCall (f, List.map (substitute_expr sigma) l)
  ;;

  let rec substitute_statement (sigma : string -> string) = function
    | Let (x, e, s) -> Let (x, e, substitute_statement sigma s)
    | Ret e -> Ret e
    | Match (x, l) ->
      Match (sigma x, List.map (fun (me, s) -> me, substitute_statement sigma s) l)
  ;;

  let rec make_substitution support image =
    match support, image with
    | [], [] -> fun x -> x
    | MatchVar x :: t1, MatchVar x' :: t2 ->
      let sigma' = make_substitution t1 t2 in
      fun y -> if x = y then x' else sigma' y
    | _ -> failwith "Impossible to make substitution, the input hasn't the right format"
  ;;

  let rec merge_match (l : (matchExpr * statement) list) : (matchExpr * statement) list =
    let lSorted = List.fast_sort (fun (me1, _) (me2, _) -> order_match_expr me1 me2) l in
    let rec merge = function
      | [] -> []
      | h :: [] -> h :: []
      | (MatchVar x1, s1) :: (MatchVar _, _) :: t -> merge ((MatchVar x1, s1) :: t)
      | (MatchTuple t1, s1) :: (MatchTuple _, _) :: t -> merge ((MatchTuple t1, s1) :: t)
      | (MatchObject (n1, mel1), s1) :: (MatchObject (n2, mel2), s2) :: t when n1 != n2 ->
        (MatchObject (n1, mel1), merge_statement s1)
        :: merge ((MatchObject (n2, mel2), s2) :: t)
      | (MatchObject (n1, mel1), s1) :: (MatchObject (_, mel2), s2) :: t ->
        let sigma = make_substitution mel2 mel1 in
        let s2' = substitute_statement sigma s2 in
        (match s1, s2' with
         | Match (x1, m1), Match (x2, m2) when x1 = x2 ->
           let m = merge_match (m1 @ m2) in
           merge ((MatchObject (n1, mel1), Match (x1, m)) :: t)
         | Match (x1, m1), mel ->
           let m = m1 @ [ MatchVar x1, mel ] in
           merge ((MatchObject (n1, mel1), Match (x1, m)) :: t)
         | _ -> failwith "Don't know what to do yet")
      | (me, s) :: h :: t -> (me, merge_statement s) :: merge (h :: t)
    in
    merge lSorted

  and merge_statement = function
    | Let (x, e, s) -> Let (x, e, merge_statement s)
    | Ret e -> Ret e
    | Match (x, l) -> Match (x, merge_match l)
  ;;

  let merge (m : machine) : machine =
    { e = m.e
    ; t = m.t
    ; f =
        List.map
          (fun (Fun f') ->
             Fun
               { name = f'.name
               ; argumentsTyped = f'.argumentsTyped
               ; return = f'.return
               ; body = merge_statement f'.body
               })
          m.f
    ; d =
        (match m.d with
         | Delta d' ->
           Delta
             { workingType = d'.workingType
             ; argument = d'.argument
             ; rules = merge_match d'.rules
             })
    }
  ;;
end

let normalize = fun x -> x |> Desugar.desugar |> Merge.merge
