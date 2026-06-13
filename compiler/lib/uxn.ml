(* type types = Ir.types [@@deriving show] *)

type var =
  | Var of string
    (* Read(x,i) for x = < obj_tag_size | data[0] | ... | data[n] > returns data[i] *)
  | Read of var * int
[@@deriving show]

type stack = Stack of var list * var list [@@deriving show]

type exp =
  | Value of stack * var * stack
  | Obj of stack * string * exp list * stack
  | UxnCall of stack * string * exp list * stack
  | Call of stack * string * exp list * stack
(* type exp =
  | Value of var
  | Obj of string * exp list
  | UxnCall of string * exp list
  | Call of string * exp list *)
[@@deriving show]

(* type var_name = string [@@deriving show]
type type_name = string [@@deriving show] *)
(* type stack = Stack of (var_name * type_name) list [@@deriving show] *)
(* Var names list *)

type stmt =
  | End of stack * stack
  | Let of stack * string * exp * stack * stmt
  | Write of stack * (var * string) * exp * stack * stmt
  | Ret of stack * exp * stack
  | Jump of
      stack * string * exp list * stack (* Same as Call but doesn't add a return point *)
  (* | Case of var * (string * stmt) list *)
  (* Case (stack_in * var, [TAG, function_to_call, stack_out]) *)
  | Case of stack * var * (string * string * stack) list
[@@deriving show]

type decl = Fun of string * stack * stmt * stack
(* (string * string) *)
(* basic function *)
[@@deriving show]

type program = Ir.types * decl list [@@deriving show]
