type typeName =
  | Unit
  | SimpleType of string
  | PrimeType of char
  | GenericType of typeName * string
  | Cross of typeName list

(* Externals *)
type valName =
  | ValueStatic of string * typeName
  | ValueFun of string * typeName * typeName

type externals =
  { externTypes : typeName list
  ; externValues : valName list
  }

(* Types *)
type constructor = Constructor of string * typeName list
type typeDecl = Type of typeName * constructor list
type types = typeDecl list

(* Expressions *)
type expr =
  | Var of string
  | Tuple of expr list
  | Object of string * expr list
  | FunCall of string * expr list

type matchExpr =
  | MatchVar of string
  | MatchTuple of matchExpr list
  | MatchObject of string * matchExpr list

(* Statements *)
type statement =
  | Let of string * expr * statement
  | Ret of expr
  | Match of string * (matchExpr * statement) list

(* Functions *)
type funDecl =
  | Fun of
      { name : string
      ; argumentsTyped : (string * typeName) list
      ; return : typeName
      ; body : statement
      }

let funDecl_get_name = function
  | Fun f -> f.name
;;

let funDecl_get_argumentsTyped = function
  | Fun f -> f.argumentsTyped
;;

let funDecl_get_return = function
  | Fun f -> f.return
;;

let funDecl_get_body = function
  | Fun f -> f.body
;;

type functions = funDecl list

(* Delta *)
type delta =
  | Delta of
      { workingType : typeName
      ; argument : string
      ; rules : (matchExpr * statement) list
      }

let delta_get_workingType = function
  | Delta d -> d.workingType
;;

let delta_get_argument = function
  | Delta d -> d.argument
;;

let delta_get_rules = function
  | Delta d -> d.rules
;;

(* Machine *)
type machine =
  { e : externals
  ; t : types
  ; f : functions
  ; d : delta
  }
