(* module type AbstractMachine = sig
  type t
  type v
  type state
  val delta : state -> v
  val eval : t -> v
end *)

module type Environment = sig
  type 'a env

  val empty : 'a env
  val extend : string * 'a * 'a env -> 'a env
  val lookup : 'a env * string -> 'a
end

module Cek (E : Environment) = struct
  include E

  type arrow_1 =
    | Constr4 of expval env * arrow_1 * term
    | Constr3 of expval env * arrow_1 * term * string
    | Constr2 (** __protected__*)

  and expval = CLOSURE of string * term * expval env (** __protected__ *)

  and term =
    | VALUE of value
    | COMP of comp (** __protected__ *)

  and value =
    | VAR of string
    | LAM of string * term (** __protected__ *)

  and comp = APP of term * term

  (** __atomic__ *)
  let eval_value ((v : value), (e : expval env)) : expval =
    match v with
    | VAR x -> lookup (e, x)
    | LAM (x, t) -> CLOSURE (x, t, e)
  ;;

  (** __protected__ *)
  type configuration =
    | Apply_1 of arrow_1 * expval
    | Eval of term * expval env * arrow_1
    | Stop of expval
    | Main of term

  (** __main__ *)
  let step (config : configuration) : configuration =
    match config with
    | Apply_1 (Constr4 (e, k, t1), CLOSURE (x, t, e_)) ->
      Eval (t1, e, Constr3 (e_, k, t, x))
    | Apply_1 (Constr3 (e_, k, t, x), w) ->
      let e__ = extend (x, w, e_) in
      Eval (t, e__, k)
    | Apply_1 (Constr2, x) -> Stop x
    | Eval (VALUE v, e, k) ->
      let res = eval_value (v, e) in
      Apply_1 (k, res)
    | Eval (COMP (APP (t0, t1)), e, k) -> Eval (t0, e, Constr4 (e, k, t1))
    | Stop _ -> config
    | Main t -> Eval (t, empty, Constr2)
  ;;

  let step_bis (config : configuration) : configuration =
    match config with
    | Apply_1 (arrow_1, expval) ->
      (match arrow_1 with
       | Constr4 (e, k, t1) ->
         (match expval with
          | CLOSURE (x, t, e_) -> Eval (t1, e, Constr3 (e_, k, t, x)))
       | Constr3 (e_, k, t, x) ->
         let e__ = extend (x, expval, e_) in
         Eval (t, e__, k)
       | _ -> failwith "undone")
    | _ -> failwith "undode"
  ;;
end
