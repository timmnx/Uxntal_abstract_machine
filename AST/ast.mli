module BNF :
  sig
    type typeName =
        Unit
      | SimpleType of string
      | PrimeType of char
      | GenericType of typeName * string
      | Cross of typeName list
    type valNames =
        ValueStatic of string * typeName
      | ValueFun of string * typeName * typeName
    type externals = {
      externTypes : typeName list;
      externValues : valNames list;
    }
    type constructor = Constructor of string * typeName list
    type typeDecl = Type of typeName * constructor list
    type types = typeDecl list
    type expr =
        Var of string
      | Tuple of expr list
      | Object of string * expr list
      | FunCall of string * expr list
    type matchExpr =
        MatchVar of string
      | MatchTuple of matchExpr list
      | MatchObject of string * matchExpr list
    type statement =
        Let of string * expr * statement
      | Ret of expr
      | Match of string * (matchExpr * statement) list
    type funDecl =
        Fun of { name : string; arguments : string list;
          argumentsType : typeName list; return : typeName; body : statement;
        }
    type functions = funDecl list
    type delta =
        Delta of { workingType : typeName; argument : string;
          rules : (matchExpr * statement) list;
        }
    type machine = { e : externals; t : types; f : functions; d : delta; }
  end
module type AST = sig type machine val m : machine end
