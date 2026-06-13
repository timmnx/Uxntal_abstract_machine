type constructors =
  (* A type is now only defined by its constructors' list and their size *)
  | Constructors of (string * int) list
[@@deriving show]

type types = (string * constructors) list [@@deriving show]

type exp =
  | Var of string
  | Obj of string * exp list
  | Call of string * exp list
[@@deriving show]

type pat =
  (* A pattern is a single constructor with variables of default case*)
  | Pattern of string * string list
  | Default
[@@deriving show]

type body =
  | Let of string * exp * body
  | Ret of exp
  | Case of string * (pat * body) list
[@@deriving show]

type decl =
  (* basic function (name, args, body, (type_in, type_out)) *)
  | Fun of string * string list * body * (string * string)
[@@deriving show]

type program = types * decl list [@@deriving show]
