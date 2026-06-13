type decl =
  | FFun of string * string list * body (* basic function *)
  | GFun of string * pat * string list * body (* simple form of pattern matching *)

and pat =
  (* A pattern is a single constructor with variables *)
  | PCon of string * string list

and body =
  | Let of string * exp * body
  | Ret of exp
  | Call of string * exp list

and exp =
  | Var of string
  | Con of string * exp list

type program = decl list
