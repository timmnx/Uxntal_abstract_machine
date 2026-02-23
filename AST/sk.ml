module type Externals = sig
	type string
	type 'a env

	val empty : 'a env
	val extend : (string * 'a * 'a env) -> 'a env
	val lookup : ('a env * string) -> 'a
end

module type Types = sig
	include Externals
	(** __protected__ *)
	type term =
	| VALUE of value
	| APP of term * term
	and value =
	| VAR of string
	| LAM of string * term


	type expval =
	| CLOSURE  of string * term * expval env

	type arrow_1 =
	| Constr4 of expval env * arrow_1 * term
	| Constr3 of expval env * arrow_1 * term * string
	| Constr2

	(** __protected__ *)
	type configuration =
	| Apply_1 of  arrow_1 * expval
	| Eval of term * expval env * arrow_1
	| Stop of expval
	| Main of term
end;;

module Functions(T : Types) = struct
	open T
	(** __atomic__ *)
	let eval_value ((v: value), (e: expval env)): expval =
		match v with
		| VAR x -> lookup (e, x)
		| LAM (x, t) -> CLOSURE (x, t, e)
	;;
end

module Machine(T : Types) = struct
	open T
	open Functions(T)
	(** __main__ *)
	let step (config: configuration): configuration =
		match config with
		| Apply_1 (Constr4 (e, k, t1), CLOSURE (x, t, e_)) -> Eval (t1, e, Constr3 (e_, k, t, x))
		| Apply_1 (Constr3 (e_, k, t, x), w) ->
			let e__ = extend (x, w, e_) in
			Eval (t, e__, k)
		| Apply_1 (Constr2, x) -> Stop x
		| Eval (VALUE v, e, k) ->
			let res = eval_value (v, e) in
			Apply_1 (k, res)
		| Eval (APP (t0, t1), e, k) -> Eval (t0, e, Constr4 (e, k, t1))
		| Stop _ -> config
		| Main t -> Eval (t, empty, Constr2)
	;;
end