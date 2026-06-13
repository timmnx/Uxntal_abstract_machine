open Compiler
open Ast

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
      ; argumentsTyped =
          [ "v", SimpleType "value"; "e", GenericType (SimpleType "expval", "env") ]
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
              , [ MatchObject ("VALUE", [ MatchVar "x" ]); MatchVar "e"; MatchVar "k" ] )
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

let%expect_test "printing" =
  m |> AstPrinting.print_machine Format.std_formatter;
  [%expect
    {|
    type string
    type 'a env
    val empty : 'a env
    val extend : (string * 'a * 'a env) -> 'a env
    val lookup : ('a env * string) -> 'a


    type term =
      | VALUE of value
      | APP of term * term

    type value =
      | VAR of string
      | LAM of string * term

    type expval =
      | CLOSURE of string * term * expval env

    type arrow_1 =
      | Constr4 of expval env * arrow_1 * term
      | Constr3 of expval env * arrow_1 * term * string
      | Constr2

    type configuration =
      | Apply_1 of arrow_1 * expval
      | Eval of term * expval env * arrow_1
      | Stop of expval
      | Main of term



    val eval_value (v: value, e: expval env) : expval =
      match v with
      | VAR x -> begin
          lookup(e, x)
        end
      | LAM(x, t) -> begin
          CLOSURE(x, t, e)
        end


    val delta (config: configuration) : configuration =
      match config with
      | Apply_1(Constr4(e, k, t1), CLOSURE(x, t, e_)) -> begin
          Eval(t1, e, Constr3(e_, k, t, x))
        end
      | Apply_1(Constr3(e_, k, t, x), W) -> begin
          let e__ = extend(x, w, e_) in Eval(t, e__, k)
        end
      | Apply_1(Constr2, x) -> begin
          Stop x
        end
      | Eval(VALUE x, e, k) -> begin
          let res = eval_value(v, e) in Apply_1(k, res)
        end
      | Eval(APP(t0, t1), e, k) -> begin
          Eval(t0, e, Constr4(e, k, t1))
        end
      | Stop _ -> begin
          config
        end
      | Main t -> begin
          Eval(t, empty, Constr2)
        end
    |}]
;;

let%expect_test "desugar" =
  m |> AstNormalize.Desugar.desugar |> AstPrinting.print_machine Format.std_formatter;
  [%expect
    {|
    type string
    type 'a env
    val empty : 'a env
    val extend : (string * 'a * 'a env) -> 'a env
    val lookup : ('a env * string) -> 'a


    type term =
      | VALUE of value
      | APP of term * term

    type value =
      | VAR of string
      | LAM of string * term

    type expval =
      | CLOSURE of string * term * expval env

    type arrow_1 =
      | Constr4 of expval env * arrow_1 * term
      | Constr3 of expval env * arrow_1 * term * string
      | Constr2

    type configuration =
      | Apply_1 of arrow_1 * expval
      | Eval of term * expval env * arrow_1
      | Stop of expval
      | Main of term



    val eval_value (v: value, e: expval env) : expval =
      match v with
      | VAR x -> begin
          lookup(e, x)
        end
      | LAM(x, t) -> begin
          CLOSURE(x, t, e)
        end


    val delta (config: configuration) : configuration =
      match config with
      | Apply_1(%v2, %v1) -> begin
          match %v2 with
          | Constr4(e, k, t1) -> begin
              match %v1 with
              | CLOSURE(x, t, e_) -> begin
                  Eval(t1, e, Constr3(e_, k, t, x))
                end
            end
        end
      | Apply_1(%v3, W) -> begin
          match %v3 with
          | Constr3(e_, k, t, x) -> begin
              let e__ = extend(x, w, e_) in Eval(t, e__, k)
            end
        end
      | Apply_1(%v4, x) -> begin
          match %v4 with
          | Constr2 -> begin
              Stop x
            end
        end
      | Eval(%v5, e, k) -> begin
          match %v5 with
          | VALUE x -> begin
              let res = eval_value(v, e) in Apply_1(k, res)
            end
        end
      | Eval(%v6, e, k) -> begin
          match %v6 with
          | APP(t0, t1) -> begin
              Eval(t0, e, Constr4(e, k, t1))
            end
        end
      | Stop _ -> begin
          config
        end
      | Main t -> begin
          Eval(t, empty, Constr2)
        end
    |}]
;;

let%expect_test "normalize" =
  m |> AstNormalize.normalize |> AstPrinting.print_machine Format.std_formatter;
  [%expect
    {|
    type string
    type 'a env
    val empty : 'a env
    val extend : (string * 'a * 'a env) -> 'a env
    val lookup : ('a env * string) -> 'a


    type term =
      | VALUE of value
      | APP of term * term

    type value =
      | VAR of string
      | LAM of string * term

    type expval =
      | CLOSURE of string * term * expval env

    type arrow_1 =
      | Constr4 of expval env * arrow_1 * term
      | Constr3 of expval env * arrow_1 * term * string
      | Constr2

    type configuration =
      | Apply_1 of arrow_1 * expval
      | Eval of term * expval env * arrow_1
      | Stop of expval
      | Main of term



    val eval_value (v: value, e: expval env) : expval =
      match v with
      | LAM(x, t) -> begin
          CLOSURE(x, t, e)
        end
      | VAR x -> begin
          lookup(e, x)
        end


    val delta (config: configuration) : configuration =
      match config with
      | Apply_1(%v2, %v1) -> begin
          match %v2 with
          | Constr2 -> begin
              Stop x
            end
          | Constr3(e_, k, t, x) -> begin
              let e__ = extend(x, w, e_) in Eval(t, e__, k)
            end
          | Constr4(e, k, t1) -> begin
              match %v1 with
              | CLOSURE(x, t, e_) -> begin
                  Eval(t1, e, Constr3(e_, k, t, x))
                end
            end
        end
      | Eval(%v5, e, k) -> begin
          match %v5 with
          | APP(t0, t1) -> begin
              Eval(t0, e, Constr4(e, k, t1))
            end
          | VALUE x -> begin
              let res = eval_value(v, e) in Apply_1(k, res)
            end
        end
      | Main t -> begin
          Eval(t, empty, Constr2)
        end
      | Stop _ -> begin
          config
        end
    |}]
;;

let%expect_test "ir" =
  m
  |> AstNormalize.normalize
  |> IrFromAstNormalized.translate_to_ir
  |> IrPrinting.program_pp Format.std_formatter;
  [%expect
    {|
    types(term ::= Constructors(VALUE(1)  :: APP(2)  :: [])
          :: value ::= Constructors(VAR(1)  :: LAM(2)  :: [])
          :: expval ::= Constructors(CLOSURE(3)  :: [])
          :: arrow_1 ::= Constructors(Constr4(3)  :: Constr3(4)  :: Constr2(0)  :: [])
          :: configuration ::= Constructors(Apply_1(2)  :: Eval(3)  :: Stop(1)  :: Main(1)  :: [])
          :: [])

    <expval>env -> expval
    Fun (eval_value, v  :: e  :: [],
      Case (v,
          Pattern(LAM, v@0  :: v@1  :: []) ->
             Ret Obj(CLOSURE, Var v@0  :: Var v@1  :: Var e  :: [])
          :: Pattern(VAR, v@0  :: []) ->
             Ret Call(lookup, Var e  :: Var v@0  :: [])
          :: [])
    )
    :: configuration -> configuration
       Fun (step, config  :: [],
         Case (config,
             Pattern(Apply_1, config@0  :: config@1  :: []) ->
                Case (config@0,
                    Pattern(Constr2, []) ->
                       Ret Obj(Stop, Var x  :: [])
                    :: Pattern(Constr3, config@0@0  :: config@0@1  :: config@0@2  :: config@0@3  :: []) ->
                       Let (e__ , Call(extend, Var config@0@3  :: Var w  :: Var config@0@0  :: []),
                        Ret Obj(Eval, Var config@0@2  :: Var e__  :: Var config@0@1  :: [])
                       )
                    :: Pattern(Constr4, config@0@0  :: config@0@1  :: config@0@2  :: []) ->
                       Case (config@1,
                           Pattern(CLOSURE, config@1@0  :: config@1@1  :: config@1@2  :: []) ->
                              Ret Obj(Eval, Var config@0@2  :: Var config@0@0  :: Obj(Constr3, Var config@1@2  :: Var config@0@1  :: Var config@1@1  :: Var config@1@0  :: [])  :: [])
                           :: [])
                    :: [])
             :: Pattern(Eval, config@0  :: config@1  :: config@2  :: []) ->
                Case (config@0,
                    Pattern(APP, config@0@0  :: config@0@1  :: []) ->
                       Ret Obj(Eval, Var config@0@0  :: Var config@1  :: Obj(Constr4, Var config@1  :: Var config@2  :: Var config@0@1  :: [])  :: [])
                    :: Pattern(VALUE, config@0@0  :: []) ->
                       Let (res , Call(eval_value, Var v  :: Var config@1  :: []),
                        Ret Obj(Apply_1, Var config@2  :: Var res  :: [])
                       )
                    :: [])
             :: Pattern(Main, config@0  :: []) ->
                Ret Obj(Eval, Var config@0  :: Var empty  :: Obj(Constr2, [])  :: [])
             :: Pattern(Stop, config@0  :: []) ->
                Ret Var config
             :: [])
       )
    :: []
    |}]
;;
