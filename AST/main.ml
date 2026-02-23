open Ast

module Machine : AST = struct
  include BNF

  let e : externals =
    { externTypes = [ SimpleType "string"; GenericType (PrimeType 'a', "env") ]
    ; externValues =
        [ ValueStatic ("empty", GenericType (PrimeType 'a', "env"))
        ; ValueFun
            ( "extend"
            , Cross
                [ SimpleType "string"; PrimeType 'a'; GenericType (PrimeType 'a', "env") ]
            , GenericType (PrimeType 'a', "env") )
        ; ValueFun
            ( "lookup"
            , Cross [ GenericType (PrimeType 'a', "env"); SimpleType "string" ]
            , PrimeType 'a' )
        ]
    }
  ;;

  let t : types =
    [ Type
        ( SimpleType "term"
        , [ Constructor ("VALUE", [ SimpleType "value" ])
          ; Constructor ("APP", [ SimpleType "term"; SimpleType "term" ])
          ] )
    ; Type
        ( SimpleType "value"
        , [ Constructor ("VAR", [ SimpleType "string" ])
          ; Constructor ("LAM", [ SimpleType "string"; SimpleType "term" ])
          ] )
    ; Type
        ( SimpleType "expval"
        , [ Constructor
              ( "CLOSURE"
              , [ SimpleType "string"
                ; SimpleType "term"
                ; GenericType (SimpleType "expval", "env")
                ] )
          ] )
    ; Type
        ( SimpleType "arrow_1"
        , [ Constructor
              ( "Constr4"
              , [ GenericType (SimpleType "expval", "env")
                ; SimpleType "arrow_1"
                ; SimpleType "term"
                ] )
          ; Constructor
              ( "Constr3"
              , [ GenericType (SimpleType "expval", "env")
                ; SimpleType "arrow_1"
                ; SimpleType "term"
                ; SimpleType "string"
                ] )
          ; Constructor ("Constr2", [])
          ] )
    ; Type
        ( SimpleType "configuration"
        , [ Constructor ("Apply_1", [ SimpleType "arrow_1"; SimpleType "expval" ])
          ; Constructor
              ( "Eval"
              , [ SimpleType "term"
                ; GenericType (SimpleType "expval", "env")
                ; SimpleType "arrow_1"
                ] )
          ; Constructor ("Stop", [ SimpleType "expval" ])
          ; Constructor ("Main", [ SimpleType "term" ])
          ] )
    ]
  ;;

  let f : functions =
    [ Fun
        { name = "eval_value"
        ; arguments = [ "v"; "e" ]
        ; argumentsType = [ SimpleType "value"; GenericType (SimpleType "expval", "env") ]
        ; return = SimpleType "expval"
        ; body =
            Match
              ( "v"
              , [ ( MatchObject ("VAR", [ MatchVar "x" ])
                  , Ret (FunCall ("lookup", [ Var "e"; Var "x" ])) )
                ; ( MatchObject ("LAM", [ MatchVar "x"; MatchVar "t" ])
                  , Ret (Object ("CLOSURE", [ Var "x"; Var "t"; Var "e" ])) )
                ] )
        }
    ]
  ;;

  let d : delta =
    Delta
      { workingType = SimpleType "configuration"
      ; argument = "config"
      ; rules =
          [ ( MatchObject
                ( "Apply_1"
                , [ MatchObject ("Constr4", [ MatchVar "e"; MatchVar "k"; MatchVar "t1" ])
                  ; MatchObject ("CLOSURE", [ MatchVar "x"; MatchVar "t"; MatchVar "e_" ])
                  ] )
            , Ret
                (Object
                   ( "Eval"
                   , [ Var "t1"
                     ; Var "e"
                     ; Object ("Constr3", [ Var "e_"; Var "k"; Var "t"; Var "x" ])
                     ] )) )
          ; ( MatchObject
                ( "Apply_1"
                , [ MatchObject
                      ( "Constr3"
                      , [ MatchVar "e_"; MatchVar "k"; MatchVar "t"; MatchVar "x" ] )
                  ; MatchVar "W"
                  ] )
            , Let
                ( "e__"
                , FunCall ("extend", [ Var "x"; Var "w"; Var "e_" ])
                , Ret (Object ("Eval", [ Var "t"; Var "e__"; Var "k" ])) ) )
          ; ( MatchObject ("Apply_1", [ MatchObject ("Constr2", []); MatchVar "x" ])
            , Ret (Object ("Stop", [ Var "x" ])) )
          ; ( MatchObject
                ( "Eval"
                , [ MatchObject ("VALUE", [ MatchVar "x" ]); MatchVar "e"; MatchVar "k" ]
                )
            , Let
                ( "res"
                , FunCall ("eval_value", [ Var "v"; Var "e" ])
                , Ret (Object ("Apply_1", [ Var "k"; Var "res" ])) ) )
          ; ( MatchObject
                ( "Eval"
                , [ MatchObject ("APP", [ MatchVar "t0"; MatchVar "t1" ])
                  ; MatchVar "e"
                  ; MatchVar "k"
                  ] )
            , Ret
                (Object
                   ( "Eval"
                   , [ Var "t0"
                     ; Var "e"
                     ; Object ("Constr4", [ Var "e"; Var "k"; Var "t1" ])
                     ] )) )
          ; MatchObject ("Stop", [ MatchVar "_" ]), Ret (Var "config")
          ; ( MatchObject ("Main", [ MatchVar "t" ])
            , Ret (Object ("Eval", [ Var "t"; Var "empty"; Object ("Constr2", []) ])) )
          ]
      }
  ;;

  let m : machine = { e; t; f; d }
end
