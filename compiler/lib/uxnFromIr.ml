open Uxn

module Working = struct
  (* let t : types ref = ref [] *)
  let res : decl list ref = ref []
  let add (d : decl) : unit = res := d :: !res

  let reset () =
    (* t := []; *)
    res := []
  ;;
end

(** [manipulate_stack s v] manipulates the stacks such that
    the variable v is added on top of the working stack,
    without changing the order the elements below.
*)
(* let manipulate_stack (s : stack) (v : var) : stack = s *)

(* ToDo : Manipulate the stack to have the arguments at the right place *)
let from_Var (x : string) : var =
  if String.contains x '@'
  then begin
    match String.split_on_char '@' x with
    | x' :: indices ->
      List.fold_left (fun acc i -> Read (acc, int_of_string i)) (Var x') indices
    | _ -> failwith "ToDo"
  end
  else Var x
;;

let exp_stacks : exp -> stack * stack = function
  | Value (s_in, _, s_out)
  | Obj (s_in, _, _, s_out)
  | UxnCall (s_in, _, _, s_out)
  | Call (s_in, _, _, s_out) -> s_in, s_out
;;

let stmt_stacks : stmt -> stack * stack = function
  | End (s_in, s_out)
  | Let (s_in, _, _, s_out, _)
  | Write (s_in, _, _, s_out, _)
  | Ret (s_in, _, s_out)
  | Jump (s_in, _, _, s_out) -> s_in, s_out
  | Case (s_in, _, in_f_args_out) ->
    let l = List.map (fun (_, _, s_out) -> s_out) in_f_args_out in
    assert (List.for_all (( = ) (List.hd l)) l);
    s_in, List.hd l
;;

let empty_stack : stack = Stack ([], [])
let push_ws (x : var) (Stack (ws, rs)) : stack = Stack (x :: ws, rs)
let var_on_top (v : var) (Stack (ws, rs)) = match v, ws with
| Var x, Var x' :: t -> x = x'
| _ -> false

(* pass the stack to annotate expressions *)
let rec from_exp (s : stack) : Ir.exp -> exp = function
  (* let rec from_exp = function *)
  | Ir.Var x ->
    let v = from_Var x in
    Value (s, v, push_ws v s)
  | Ir.Obj (name, args) -> begin
    let new_args, s'' =
      List.fold_right
        (fun e (acc, s') ->
           let e' = from_exp s' e in
           e' :: acc, exp_stacks e' |> snd)
        args
        ([], s)
    in
    Obj (s'', name, new_args, push_ws (from_Var name) s)
  end
  | Ir.Call (f, args) when String.starts_with ~prefix:"@uxn." f ->
    (* UxnCall (String.sub f 5 (String.length f - 5), List.map from_exp args) *)
    begin
      let f = String.sub f 5 (String.length f - 5) in
      let new_args, _ =
        List.fold_left
          (fun (acc, s') e ->
             let e' = from_exp s' e in
             acc @ [e'], exp_stacks e' |> snd)
          ([], s)
          args
      in
      UxnCall (s, f, new_args, push_ws (from_Var f) s)
    end
  | Ir.Call (f, args) -> begin
    let new_args, _ =
      List.fold_left
        (fun (acc, s) e ->
           let e' = from_exp s e in
           acc @ [e'], exp_stacks e' |> snd)
        ([], s)
        args
    in
    Call (s, f, new_args, push_ws (from_Var f) s)
  end
;;

let rec from_body (s : stack) (context : string) type_in_out : Ir.body -> stmt = function
  | Ir.Let (x, e, b) -> begin
    let s' = push_ws (Var x) s in
    let b' = from_body s context type_in_out b in
    Let (s, x, from_exp s e, s', b')
  end
  | Ir.Ret e -> begin Ret (s, from_exp s e, push_ws (Var "%ret") empty_stack) end
  | Ir.Case (x, pat_body_list) -> begin
    let v = from_Var x in
    let s' = if var_on_top v s then s else push_ws v s in
    let iter (p, b) =
      let cons_name =
        match p with
        | Ir.Pattern (c_n, _) -> c_n
        | Default -> "Default"
      in
      let f = Format.sprintf "%s<%s>" context cons_name in
      (* Il faut décider de la norme d'appel sur la pile *)
      (* Pour le moment, le top de pile est le top de list *)
      Ir.Fun (f, [], b, type_in_out) |> from_decl s' |> Working.add;
      cons_name, f, s'
    in
    Case (s, v, List.map iter pat_body_list)
  end

and from_decl (s : stack) (Ir.Fun (name, args, b, type_in_out)) : decl =
  let s' = List.fold_right (fun x s_acc -> push_ws (Var x) s_acc) args s in
  let b' = from_body s' name type_in_out b in
  Fun (name, s', b', stmt_stacks b' |> snd)
;;

let from_program (t, p) =
  Working.reset ();
  List.iter (fun f -> from_decl empty_stack f |> Working.add) p;
  t, !Working.res
;;
