open Compiler
open Ast


let e : externals =
  { externTypes = [ SimpleType "@uxn.short" ]
  ; externValues =
      [ ValueFun
          ( "@uxn.ADD2"
          , Cross [ SimpleType "@uxn.short"; SimpleType "@uxn.short" ]
          , SimpleType "@uxn.short" )
      ; ValueFun
          ( "@uxn.SUB2"
          , Cross [ SimpleType "@uxn.short"; SimpleType "@uxn.short" ]
          , SimpleType "@uxn.short" )
      ]
  }
;;

let t : types =
  [ Type
      ( SimpleType "expr"
      , [ Constructor ("Int", [ SimpleType "@uxn.short" ])
        ; Constructor ("Add", [ SimpleType "expr"; SimpleType "expr" ])
        ; Constructor ("Sub", [ SimpleType "expr"; SimpleType "expr" ])
        ] )
  ; Type
      ( SimpleType "cont"
      , [ Constructor ("Id", [])
        ; Constructor ("ArAdd", [ SimpleType "expr"; SimpleType "cont" ])
        ; Constructor ("FnAdd", [ SimpleType "expr"; SimpleType "cont" ])
        ; Constructor ("ArSub", [ SimpleType "expr"; SimpleType "cont" ])
        ; Constructor ("FnSub", [ SimpleType "expr"; SimpleType "cont" ])
        ] )
  ; Type
      ( SimpleType "configuration"
      , [ Constructor ("Eval", [ SimpleType "expr"; SimpleType "cont" ])
        ; Constructor ("Apply", [ SimpleType "cont"; SimpleType "expr" ])
        ; Constructor ("Stop", [ SimpleType "@uxn.short" ])
        ; Constructor ("Main", [ SimpleType "@uxn.short" ])
        ] )
  ]
;;

let f : functions = []

let d : delta =
  Delta
    { workingType = SimpleType "configuration"
    ; argument = "config"
    ; rules =
        [ ( MatchObject
              ("Eval", [ MatchObject ("Int", [ MatchVar "i" ]); MatchVar "kappa" ])
          , Ret (Object ("Apply", [ Var "kappa"; Object ("Int", [ Var "i" ]) ])) )
        ; ( MatchObject
              ( "Eval"
              , [ MatchObject ("Add", [ MatchVar "e1"; MatchVar "e2" ])
                ; MatchVar "kappa"
                ] )
          , Ret
              (Object ("Eval", [ Var "e1"; Object ("ArAdd", [ Var "e2"; Var "kappa" ]) ]))
          )
        ; ( MatchObject
              ( "Eval"
              , [ MatchObject ("Sub", [ MatchVar "e1"; MatchVar "e2" ])
                ; MatchVar "kappa"
                ] )
          , Ret
              (Object ("Eval", [ Var "e1"; Object ("ArSub", [ Var "e2"; Var "kappa" ]) ]))
          )
        ; ( MatchObject
              ("Apply", [ MatchObject ("Id", []); MatchObject ("Int", [ MatchVar "i" ]) ])
          , Ret (Object ("Stop", [ Var "i" ])) )
        ; ( MatchObject
              ( "Apply"
              , [ MatchObject ("ArAdd", [ MatchVar "e"; MatchVar "kappa" ])
                ; MatchObject ("Int", [ MatchVar "i" ])
                ] )
          , Ret
              (Object
                 ( "Eval"
                 , [ Var "e"
                   ; Object ("FnAdd", [ Object ("Int", [ Var "i" ]); Var "kappa" ])
                   ] )) )
        ; ( MatchObject
              ( "Apply"
              , [ MatchObject ("ArSub", [ MatchVar "e"; MatchVar "kappa" ])
                ; MatchObject ("Int", [ MatchVar "i" ])
                ] )
          , Ret
              (Object
                 ( "Eval"
                 , [ Var "e"
                   ; Object ("FnSub", [ Object ("Int", [ Var "i" ]); Var "kappa" ])
                   ] )) )
        ; ( MatchObject
              ( "Apply"
              , [ MatchObject
                    ("FnAdd", [ MatchObject ("Int", [ MatchVar "i1" ]); MatchVar "kappa" ])
                ; MatchObject ("Int", [ MatchVar "i2" ])
                ] )
          , Ret
              (Object
                 ( "Eval"
                 , [ Object ("Int", [ FunCall ("@uxn.ADD2", [ Var "i1"; Var "i2" ]) ])
                   ; Var "kappa"
                   ] )) )
        ; ( MatchObject
              ( "Apply"
              , [ MatchObject
                    ("FnSub", [ MatchObject ("Int", [ MatchVar "i1" ]); MatchVar "kappa" ])
                ; MatchObject ("Int", [ MatchVar "i2" ])
                ] )
          , Ret
              (Object
                 ( "Eval"
                 , [ Object ("Int", [ FunCall ("@uxn.SUB2", [ Var "i1"; Var "i2" ]) ])
                   ; Var "kappa"
                   ] )) )
        ; MatchObject ("Stop", [ MatchVar "i" ]), Ret (Object ("Stop", [ Var "i" ]))
        ; ( MatchObject ("Main", [ MatchVar "i" ])
          , Ret (Object ("Eval", [ Object ("Int", [ Var "i" ]); Object ("Id", []) ])) )
        ]
    }
;;


let m : machine = { e; t; f; d }
