open Compiler
open Ast
open Machine2

let%expect_test "printing" =
  m |> AstPrinting.print_machine Format.std_formatter;
  [%expect
    {|
    type @uxn.short
    val @uxn.ADD2 : (@uxn.short * @uxn.short) -> @uxn.short
    val @uxn.SUB2 : (@uxn.short * @uxn.short) -> @uxn.short


    type expr =
      | Int of @uxn.short
      | Add of expr * expr
      | Sub of expr * expr

    type cont =
      | Id
      | ArAdd of expr * cont
      | FnAdd of expr * cont
      | ArSub of expr * cont
      | FnSub of expr * cont

    type configuration =
      | Eval of expr * cont
      | Apply of cont * expr
      | Stop of @uxn.short
      | Main of @uxn.short





    val delta (config: configuration) : configuration =
      match config with
      | Eval(Int i, kappa) -> begin
          Apply(kappa, Int i)
        end
      | Eval(Add(e1, e2), kappa) -> begin
          Eval(e1, ArAdd(e2, kappa))
        end
      | Eval(Sub(e1, e2), kappa) -> begin
          Eval(e1, ArSub(e2, kappa))
        end
      | Apply(Id, Int i) -> begin
          Stop i
        end
      | Apply(ArAdd(e, kappa), Int i) -> begin
          Eval(e, FnAdd(Int i, kappa))
        end
      | Apply(ArSub(e, kappa), Int i) -> begin
          Eval(e, FnSub(Int i, kappa))
        end
      | Apply(FnAdd(Int i1, kappa), Int i2) -> begin
          Eval(Int @uxn.ADD2(i1, i2), kappa)
        end
      | Apply(FnSub(Int i1, kappa), Int i2) -> begin
          Eval(Int @uxn.SUB2(i1, i2), kappa)
        end
      | Stop i -> begin
          Stop i
        end
      | Main i -> begin
          Eval(Int i, Id)
        end
    |}]
;;

let%expect_test "desugar" =
  m |> AstNormalize.Desugar.desugar |> AstPrinting.print_machine Format.std_formatter;
  [%expect
    {|
    type @uxn.short
    val @uxn.ADD2 : (@uxn.short * @uxn.short) -> @uxn.short
    val @uxn.SUB2 : (@uxn.short * @uxn.short) -> @uxn.short


    type expr =
      | Int of @uxn.short
      | Add of expr * expr
      | Sub of expr * expr

    type cont =
      | Id
      | ArAdd of expr * cont
      | FnAdd of expr * cont
      | ArSub of expr * cont
      | FnSub of expr * cont

    type configuration =
      | Eval of expr * cont
      | Apply of cont * expr
      | Stop of @uxn.short
      | Main of @uxn.short





    val delta (config: configuration) : configuration =
      match config with
      | Eval(%v1, kappa) -> begin
          match %v1 with
          | Int i -> begin
              Apply(kappa, Int i)
            end
        end
      | Eval(%v2, kappa) -> begin
          match %v2 with
          | Add(e1, e2) -> begin
              Eval(e1, ArAdd(e2, kappa))
            end
        end
      | Eval(%v3, kappa) -> begin
          match %v3 with
          | Sub(e1, e2) -> begin
              Eval(e1, ArSub(e2, kappa))
            end
        end
      | Apply(%v5, %v4) -> begin
          match %v5 with
          | Id -> begin
              match %v4 with
              | Int i -> begin
                  Stop i
                end
            end
        end
      | Apply(%v7, %v6) -> begin
          match %v7 with
          | ArAdd(e, kappa) -> begin
              match %v6 with
              | Int i -> begin
                  Eval(e, FnAdd(Int i, kappa))
                end
            end
        end
      | Apply(%v9, %v8) -> begin
          match %v9 with
          | ArSub(e, kappa) -> begin
              match %v8 with
              | Int i -> begin
                  Eval(e, FnSub(Int i, kappa))
                end
            end
        end
      | Apply(%v11, %v10) -> begin
          match %v11 with
          | FnAdd(%v12, kappa) -> begin
              match %v12 with
              | Int i1 -> begin
                  match %v10 with
                  | Int i2 -> begin
                      Eval(Int @uxn.ADD2(i1, i2), kappa)
                    end
                end
            end
        end
      | Apply(%v14, %v13) -> begin
          match %v14 with
          | FnSub(%v15, kappa) -> begin
              match %v15 with
              | Int i1 -> begin
                  match %v13 with
                  | Int i2 -> begin
                      Eval(Int @uxn.SUB2(i1, i2), kappa)
                    end
                end
            end
        end
      | Stop i -> begin
          Stop i
        end
      | Main i -> begin
          Eval(Int i, Id)
        end
    |}]
;;

let%expect_test "normalize" =
  m |> AstNormalize.normalize |> AstPrinting.print_machine Format.std_formatter;
  [%expect
    {|
    type @uxn.short
    val @uxn.ADD2 : (@uxn.short * @uxn.short) -> @uxn.short
    val @uxn.SUB2 : (@uxn.short * @uxn.short) -> @uxn.short


    type expr =
      | Int of @uxn.short
      | Add of expr * expr
      | Sub of expr * expr

    type cont =
      | Id
      | ArAdd of expr * cont
      | FnAdd of expr * cont
      | ArSub of expr * cont
      | FnSub of expr * cont

    type configuration =
      | Eval of expr * cont
      | Apply of cont * expr
      | Stop of @uxn.short
      | Main of @uxn.short





    val delta (config: configuration) : configuration =
      match config with
      | Apply(%v5, %v4) -> begin
          match %v5 with
          | ArAdd(e, kappa) -> begin
              match %v4 with
              | Int i -> begin
                  Eval(e, FnAdd(Int i, kappa))
                end
            end
          | ArSub(e, kappa) -> begin
              match %v4 with
              | Int i -> begin
                  Eval(e, FnSub(Int i, kappa))
                end
            end
          | FnAdd(%v12, kappa) -> begin
              match %v12 with
              | Int i1 -> begin
                  match %v4 with
                  | Int i2 -> begin
                      Eval(Int @uxn.ADD2(i1, i2), kappa)
                    end
                end
            end
          | FnSub(%v15, kappa) -> begin
              match %v15 with
              | Int i1 -> begin
                  match %v4 with
                  | Int i2 -> begin
                      Eval(Int @uxn.SUB2(i1, i2), kappa)
                    end
                end
            end
          | Id -> begin
              match %v4 with
              | Int i -> begin
                  Stop i
                end
            end
        end
      | Eval(%v1, kappa) -> begin
          match %v1 with
          | Add(e1, e2) -> begin
              Eval(e1, ArAdd(e2, kappa))
            end
          | Int i -> begin
              Apply(kappa, Int i)
            end
          | Sub(e1, e2) -> begin
              Eval(e1, ArSub(e2, kappa))
            end
        end
      | Main i -> begin
          Eval(Int i, Id)
        end
      | Stop i -> begin
          Stop i
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
    types(expr ::= Constructors(Int(1)  :: Add(2)  :: Sub(2)  :: [])
          :: cont ::= Constructors(Id(0)  :: ArAdd(2)  :: FnAdd(2)  :: ArSub(2)  :: FnSub(2)  :: [])
          :: configuration ::= Constructors(Eval(2)  :: Apply(2)  :: Stop(1)  :: Main(1)  :: [])
          :: [])

    configuration -> configuration
    Fun (step, config  :: [],
      Case (config,
          Pattern(Apply, config@0  :: config@1  :: []) ->
             Case (config@0,
                 Pattern(ArAdd, config@0@0  :: config@0@1  :: []) ->
                    Case (config@1,
                        Pattern(Int, config@1@0  :: []) ->
                           Ret Obj(Eval, Var config@0@0  :: Obj(FnAdd, Obj(Int, Var config@1@0  :: [])  :: Var config@0@1  :: [])  :: [])
                        :: [])
                 :: Pattern(ArSub, config@0@0  :: config@0@1  :: []) ->
                    Case (config@1,
                        Pattern(Int, config@1@0  :: []) ->
                           Ret Obj(Eval, Var config@0@0  :: Obj(FnSub, Obj(Int, Var config@1@0  :: [])  :: Var config@0@1  :: [])  :: [])
                        :: [])
                 :: Pattern(FnAdd, config@0@0  :: config@0@1  :: []) ->
                    Case (config@0@0,
                        Pattern(Int, config@0@0@0  :: []) ->
                           Case (config@1,
                               Pattern(Int, config@1@0  :: []) ->
                                  Ret Obj(Eval, Obj(Int, Call(@uxn.ADD2, Var config@0@0@0  :: Var config@1@0  :: [])  :: [])  :: Var config@0@1  :: [])
                               :: [])
                        :: [])
                 :: Pattern(FnSub, config@0@0  :: config@0@1  :: []) ->
                    Case (config@0@0,
                        Pattern(Int, config@0@0@0  :: []) ->
                           Case (config@1,
                               Pattern(Int, config@1@0  :: []) ->
                                  Ret Obj(Eval, Obj(Int, Call(@uxn.SUB2, Var config@0@0@0  :: Var config@1@0  :: [])  :: [])  :: Var config@0@1  :: [])
                               :: [])
                        :: [])
                 :: Pattern(Id, []) ->
                    Case (config@1,
                        Pattern(Int, config@1@0  :: []) ->
                           Ret Obj(Stop, Var config@1@0  :: [])
                        :: [])
                 :: [])
          :: Pattern(Eval, config@0  :: config@1  :: []) ->
             Case (config@0,
                 Pattern(Add, config@0@0  :: config@0@1  :: []) ->
                    Ret Obj(Eval, Var config@0@0  :: Obj(ArAdd, Var config@0@1  :: Var config@1  :: [])  :: [])
                 :: Pattern(Int, config@0@0  :: []) ->
                    Ret Obj(Apply, Var config@1  :: Obj(Int, Var config@0@0  :: [])  :: [])
                 :: Pattern(Sub, config@0@0  :: config@0@1  :: []) ->
                    Ret Obj(Eval, Var config@0@0  :: Obj(ArSub, Var config@0@1  :: Var config@1  :: [])  :: [])
                 :: [])
          :: Pattern(Main, config@0  :: []) ->
             Ret Obj(Eval, Obj(Int, Var config@0  :: [])  :: Obj(Id, [])  :: [])
          :: Pattern(Stop, config@0  :: []) ->
             Ret Obj(Stop, Var config@0  :: [])
          :: [])
    )
    :: []
    |}]
;;

let%expect_test "uxn" =
  m
  |> AstNormalize.normalize
  |> IrFromAstNormalized.translate_to_ir
  |> UxnFromIr.from_program
  |> Uxn.pp_program Format.std_formatter;
  [%expect
    {|
    ([("expr", (Ir.Constructors [("Int", 1); ("Add", 2); ("Sub", 2)]));
       ("cont",
        (Ir.Constructors
           [("Id", 0); ("ArAdd", 2); ("FnAdd", 2); ("ArSub", 2); ("FnSub", 2)]));
       ("configuration",
        (Ir.Constructors [("Eval", 2); ("Apply", 2); ("Stop", 1); ("Main", 1)]))
       ],
     [(Uxn.Fun ("step", (Uxn.Stack ([(Uxn.Var "config")], [])),
         (Uxn.Case ((Uxn.Stack ([(Uxn.Var "config")], [])), (Uxn.Var "config"),
            [("Apply", "step<Apply>", (Uxn.Stack ([(Uxn.Var "config")], [])));
              ("Eval", "step<Eval>", (Uxn.Stack ([(Uxn.Var "config")], [])));
              ("Main", "step<Main>", (Uxn.Stack ([(Uxn.Var "config")], [])));
              ("Stop", "step<Stop>", (Uxn.Stack ([(Uxn.Var "config")], [])))]
            )),
         (Uxn.Stack ([(Uxn.Var "config")], []))));
       (Uxn.Fun ("step<Stop>", (Uxn.Stack ([(Uxn.Var "config")], [])),
          (Uxn.Ret ((Uxn.Stack ([(Uxn.Var "config")], [])),
             (Uxn.Obj (
                (Uxn.Stack (
                   [(Uxn.Read ((Uxn.Var "config"), 0)); (Uxn.Var "config")],
                   [])),
                "Stop",
                [(Uxn.Value ((Uxn.Stack ([(Uxn.Var "config")], [])),
                    (Uxn.Read ((Uxn.Var "config"), 0)),
                    (Uxn.Stack (
                       [(Uxn.Read ((Uxn.Var "config"), 0)); (Uxn.Var "config")],
                       []))
                    ))
                  ],
                (Uxn.Stack ([(Uxn.Var "Stop"); (Uxn.Var "config")], [])))),
             (Uxn.Stack ([(Uxn.Var "%ret")], [])))),
          (Uxn.Stack ([(Uxn.Var "%ret")], []))));
       (Uxn.Fun ("step<Main>", (Uxn.Stack ([(Uxn.Var "config")], [])),
          (Uxn.Ret ((Uxn.Stack ([(Uxn.Var "config")], [])),
             (Uxn.Obj (
                (Uxn.Stack (
                   [(Uxn.Var "Int"); (Uxn.Var "Id"); (Uxn.Var "config")],
                   [])),
                "Eval",
                [(Uxn.Obj (
                    (Uxn.Stack (
                       [(Uxn.Read ((Uxn.Var "config"), 0)); (Uxn.Var "Id");
                         (Uxn.Var "config")],
                       [])),
                    "Int",
                    [(Uxn.Value (
                        (Uxn.Stack ([(Uxn.Var "Id"); (Uxn.Var "config")], [])),
                        (Uxn.Read ((Uxn.Var "config"), 0)),
                        (Uxn.Stack (
                           [(Uxn.Read ((Uxn.Var "config"), 0)); (Uxn.Var "Id");
                             (Uxn.Var "config")],
                           []))
                        ))
                      ],
                    (Uxn.Stack (
                       [(Uxn.Var "Int"); (Uxn.Var "Id"); (Uxn.Var "config")],
                       []))
                    ));
                  (Uxn.Obj ((Uxn.Stack ([(Uxn.Var "config")], [])), "Id",
                     [], (Uxn.Stack ([(Uxn.Var "Id"); (Uxn.Var "config")], []))))
                  ],
                (Uxn.Stack ([(Uxn.Var "Eval"); (Uxn.Var "config")], [])))),
             (Uxn.Stack ([(Uxn.Var "%ret")], [])))),
          (Uxn.Stack ([(Uxn.Var "%ret")], []))));
       (Uxn.Fun ("step<Eval>", (Uxn.Stack ([(Uxn.Var "config")], [])),
          (Uxn.Case ((Uxn.Stack ([(Uxn.Var "config")], [])),
             (Uxn.Read ((Uxn.Var "config"), 0)),
             [("Add", "step<Eval><Add>",
               (Uxn.Stack (
                  [(Uxn.Read ((Uxn.Var "config"), 0)); (Uxn.Var "config")],
                  [])));
               ("Int", "step<Eval><Int>",
                (Uxn.Stack (
                   [(Uxn.Read ((Uxn.Var "config"), 0)); (Uxn.Var "config")],
                   [])));
               ("Sub", "step<Eval><Sub>",
                (Uxn.Stack (
                   [(Uxn.Read ((Uxn.Var "config"), 0)); (Uxn.Var "config")],
                   [])))
               ]
             )),
          (Uxn.Stack ([(Uxn.Read ((Uxn.Var "config"), 0)); (Uxn.Var "config")],
             []))
          ));
       (Uxn.Fun ("step<Eval><Sub>",
          (Uxn.Stack ([(Uxn.Read ((Uxn.Var "config"), 0)); (Uxn.Var "config")],
             [])),
          (Uxn.Ret (
             (Uxn.Stack (
                [(Uxn.Read ((Uxn.Var "config"), 0)); (Uxn.Var "config")],
                [])),
             (Uxn.Obj (
                (Uxn.Stack (
                   [(Uxn.Read ((Uxn.Read ((Uxn.Var "config"), 0)), 0));
                     (Uxn.Var "ArSub"); (Uxn.Read ((Uxn.Var "config"), 0));
                     (Uxn.Var "config")],
                   [])),
                "Eval",
                [(Uxn.Value (
                    (Uxn.Stack (
                       [(Uxn.Var "ArSub"); (Uxn.Read ((Uxn.Var "config"), 0));
                         (Uxn.Var "config")],
                       [])),
                    (Uxn.Read ((Uxn.Read ((Uxn.Var "config"), 0)), 0)),
                    (Uxn.Stack (
                       [(Uxn.Read ((Uxn.Read ((Uxn.Var "config"), 0)), 0));
                         (Uxn.Var "ArSub"); (Uxn.Read ((Uxn.Var "config"), 0));
                         (Uxn.Var "config")],
                       []))
                    ));
                  (Uxn.Obj (
                     (Uxn.Stack (
                        [(Uxn.Read ((Uxn.Read ((Uxn.Var "config"), 0)), 1));
                          (Uxn.Read ((Uxn.Var "config"), 1));
                          (Uxn.Read ((Uxn.Var "config"), 0)); (Uxn.Var "config")],
                        [])),
                     "ArSub",
                     [(Uxn.Value (
                         (Uxn.Stack (
                            [(Uxn.Read ((Uxn.Var "config"), 1));
                              (Uxn.Read ((Uxn.Var "config"), 0));
                              (Uxn.Var "config")],
                            [])),
                         (Uxn.Read ((Uxn.Read ((Uxn.Var "config"), 0)), 1)),
                         (Uxn.Stack (
                            [(Uxn.Read ((Uxn.Read ((Uxn.Var "config"), 0)), 1));
                              (Uxn.Read ((Uxn.Var "config"), 1));
                              (Uxn.Read ((Uxn.Var "config"), 0));
                              (Uxn.Var "config")],
                            []))
                         ));
                       (Uxn.Value (
                          (Uxn.Stack (
                             [(Uxn.Read ((Uxn.Var "config"), 0));
                               (Uxn.Var "config")],
                             [])),
                          (Uxn.Read ((Uxn.Var "config"), 1)),
                          (Uxn.Stack (
                             [(Uxn.Read ((Uxn.Var "config"), 1));
                               (Uxn.Read ((Uxn.Var "config"), 0));
                               (Uxn.Var "config")],
                             []))
                          ))
                       ],
                     (Uxn.Stack (
                        [(Uxn.Var "ArSub"); (Uxn.Read ((Uxn.Var "config"), 0));
                          (Uxn.Var "config")],
                        []))
                     ))
                  ],
                (Uxn.Stack (
                   [(Uxn.Var "Eval"); (Uxn.Read ((Uxn.Var "config"), 0));
                     (Uxn.Var "config")],
                   []))
                )),
             (Uxn.Stack ([(Uxn.Var "%ret")], [])))),
          (Uxn.Stack ([(Uxn.Var "%ret")], []))));
       (Uxn.Fun ("step<Eval><Int>",
          (Uxn.Stack ([(Uxn.Read ((Uxn.Var "config"), 0)); (Uxn.Var "config")],
             [])),
          (Uxn.Ret (
             (Uxn.Stack (
                [(Uxn.Read ((Uxn.Var "config"), 0)); (Uxn.Var "config")],
                [])),
             (Uxn.Obj (
                (Uxn.Stack (
                   [(Uxn.Read ((Uxn.Var "config"), 1)); (Uxn.Var "Int");
                     (Uxn.Read ((Uxn.Var "config"), 0)); (Uxn.Var "config")],
                   [])),
                "Apply",
                [(Uxn.Value (
                    (Uxn.Stack (
                       [(Uxn.Var "Int"); (Uxn.Read ((Uxn.Var "config"), 0));
                         (Uxn.Var "config")],
                       [])),
                    (Uxn.Read ((Uxn.Var "config"), 1)),
                    (Uxn.Stack (
                       [(Uxn.Read ((Uxn.Var "config"), 1)); (Uxn.Var "Int");
                         (Uxn.Read ((Uxn.Var "config"), 0)); (Uxn.Var "config")],
                       []))
                    ));
                  (Uxn.Obj (
                     (Uxn.Stack (
                        [(Uxn.Read ((Uxn.Read ((Uxn.Var "config"), 0)), 0));
                          (Uxn.Read ((Uxn.Var "config"), 0)); (Uxn.Var "config")],
                        [])),
                     "Int",
                     [(Uxn.Value (
                         (Uxn.Stack (
                            [(Uxn.Read ((Uxn.Var "config"), 0));
                              (Uxn.Var "config")],
                            [])),
                         (Uxn.Read ((Uxn.Read ((Uxn.Var "config"), 0)), 0)),
                         (Uxn.Stack (
                            [(Uxn.Read ((Uxn.Read ((Uxn.Var "config"), 0)), 0));
                              (Uxn.Read ((Uxn.Var "config"), 0));
                              (Uxn.Var "config")],
                            []))
                         ))
                       ],
                     (Uxn.Stack (
                        [(Uxn.Var "Int"); (Uxn.Read ((Uxn.Var "config"), 0));
                          (Uxn.Var "config")],
                        []))
                     ))
                  ],
                (Uxn.Stack (
                   [(Uxn.Var "Apply"); (Uxn.Read ((Uxn.Var "config"), 0));
                     (Uxn.Var "config")],
                   []))
                )),
             (Uxn.Stack ([(Uxn.Var "%ret")], [])))),
          (Uxn.Stack ([(Uxn.Var "%ret")], []))));
       (Uxn.Fun ("step<Eval><Add>",
          (Uxn.Stack ([(Uxn.Read ((Uxn.Var "config"), 0)); (Uxn.Var "config")],
             [])),
          (Uxn.Ret (
             (Uxn.Stack (
                [(Uxn.Read ((Uxn.Var "config"), 0)); (Uxn.Var "config")],
                [])),
             (Uxn.Obj (
                (Uxn.Stack (
                   [(Uxn.Read ((Uxn.Read ((Uxn.Var "config"), 0)), 0));
                     (Uxn.Var "ArAdd"); (Uxn.Read ((Uxn.Var "config"), 0));
                     (Uxn.Var "config")],
                   [])),
                "Eval",
                [(Uxn.Value (
                    (Uxn.Stack (
                       [(Uxn.Var "ArAdd"); (Uxn.Read ((Uxn.Var "config"), 0));
                         (Uxn.Var "config")],
                       [])),
                    (Uxn.Read ((Uxn.Read ((Uxn.Var "config"), 0)), 0)),
                    (Uxn.Stack (
                       [(Uxn.Read ((Uxn.Read ((Uxn.Var "config"), 0)), 0));
                         (Uxn.Var "ArAdd"); (Uxn.Read ((Uxn.Var "config"), 0));
                         (Uxn.Var "config")],
                       []))
                    ));
                  (Uxn.Obj (
                     (Uxn.Stack (
                        [(Uxn.Read ((Uxn.Read ((Uxn.Var "config"), 0)), 1));
                          (Uxn.Read ((Uxn.Var "config"), 1));
                          (Uxn.Read ((Uxn.Var "config"), 0)); (Uxn.Var "config")],
                        [])),
                     "ArAdd",
                     [(Uxn.Value (
                         (Uxn.Stack (
                            [(Uxn.Read ((Uxn.Var "config"), 1));
                              (Uxn.Read ((Uxn.Var "config"), 0));
                              (Uxn.Var "config")],
                            [])),
                         (Uxn.Read ((Uxn.Read ((Uxn.Var "config"), 0)), 1)),
                         (Uxn.Stack (
                            [(Uxn.Read ((Uxn.Read ((Uxn.Var "config"), 0)), 1));
                              (Uxn.Read ((Uxn.Var "config"), 1));
                              (Uxn.Read ((Uxn.Var "config"), 0));
                              (Uxn.Var "config")],
                            []))
                         ));
                       (Uxn.Value (
                          (Uxn.Stack (
                             [(Uxn.Read ((Uxn.Var "config"), 0));
                               (Uxn.Var "config")],
                             [])),
                          (Uxn.Read ((Uxn.Var "config"), 1)),
                          (Uxn.Stack (
                             [(Uxn.Read ((Uxn.Var "config"), 1));
                               (Uxn.Read ((Uxn.Var "config"), 0));
                               (Uxn.Var "config")],
                             []))
                          ))
                       ],
                     (Uxn.Stack (
                        [(Uxn.Var "ArAdd"); (Uxn.Read ((Uxn.Var "config"), 0));
                          (Uxn.Var "config")],
                        []))
                     ))
                  ],
                (Uxn.Stack (
                   [(Uxn.Var "Eval"); (Uxn.Read ((Uxn.Var "config"), 0));
                     (Uxn.Var "config")],
                   []))
                )),
             (Uxn.Stack ([(Uxn.Var "%ret")], [])))),
          (Uxn.Stack ([(Uxn.Var "%ret")], []))));
       (Uxn.Fun ("step<Apply>", (Uxn.Stack ([(Uxn.Var "config")], [])),
          (Uxn.Case ((Uxn.Stack ([(Uxn.Var "config")], [])),
             (Uxn.Read ((Uxn.Var "config"), 0)),
             [("ArAdd", "step<Apply><ArAdd>",
               (Uxn.Stack (
                  [(Uxn.Read ((Uxn.Var "config"), 0)); (Uxn.Var "config")],
                  [])));
               ("ArSub", "step<Apply><ArSub>",
                (Uxn.Stack (
                   [(Uxn.Read ((Uxn.Var "config"), 0)); (Uxn.Var "config")],
                   [])));
               ("FnAdd", "step<Apply><FnAdd>",
                (Uxn.Stack (
                   [(Uxn.Read ((Uxn.Var "config"), 0)); (Uxn.Var "config")],
                   [])));
               ("FnSub", "step<Apply><FnSub>",
                (Uxn.Stack (
                   [(Uxn.Read ((Uxn.Var "config"), 0)); (Uxn.Var "config")],
                   [])));
               ("Id", "step<Apply><Id>",
                (Uxn.Stack (
                   [(Uxn.Read ((Uxn.Var "config"), 0)); (Uxn.Var "config")],
                   [])))
               ]
             )),
          (Uxn.Stack ([(Uxn.Read ((Uxn.Var "config"), 0)); (Uxn.Var "config")],
             []))
          ));
       (Uxn.Fun ("step<Apply><Id>",
          (Uxn.Stack ([(Uxn.Read ((Uxn.Var "config"), 0)); (Uxn.Var "config")],
             [])),
          (Uxn.Case (
             (Uxn.Stack (
                [(Uxn.Read ((Uxn.Var "config"), 0)); (Uxn.Var "config")],
                [])),
             (Uxn.Read ((Uxn.Var "config"), 1)),
             [("Int", "step<Apply><Id><Int>",
               (Uxn.Stack (
                  [(Uxn.Read ((Uxn.Var "config"), 1));
                    (Uxn.Read ((Uxn.Var "config"), 0)); (Uxn.Var "config")],
                  [])))
               ]
             )),
          (Uxn.Stack (
             [(Uxn.Read ((Uxn.Var "config"), 1));
               (Uxn.Read ((Uxn.Var "config"), 0)); (Uxn.Var "config")],
             []))
          ));
       (Uxn.Fun ("step<Apply><Id><Int>",
          (Uxn.Stack (
             [(Uxn.Read ((Uxn.Var "config"), 1));
               (Uxn.Read ((Uxn.Var "config"), 0)); (Uxn.Var "config")],
             [])),
          (Uxn.Ret (
             (Uxn.Stack (
                [(Uxn.Read ((Uxn.Var "config"), 1));
                  (Uxn.Read ((Uxn.Var "config"), 0)); (Uxn.Var "config")],
                [])),
             (Uxn.Obj (
                (Uxn.Stack (
                   [(Uxn.Read ((Uxn.Read ((Uxn.Var "config"), 1)), 0));
                     (Uxn.Read ((Uxn.Var "config"), 1));
                     (Uxn.Read ((Uxn.Var "config"), 0)); (Uxn.Var "config")],
                   [])),
                "Stop",
                [(Uxn.Value (
                    (Uxn.Stack (
                       [(Uxn.Read ((Uxn.Var "config"), 1));
                         (Uxn.Read ((Uxn.Var "config"), 0)); (Uxn.Var "config")],
                       [])),
                    (Uxn.Read ((Uxn.Read ((Uxn.Var "config"), 1)), 0)),
                    (Uxn.Stack (
                       [(Uxn.Read ((Uxn.Read ((Uxn.Var "config"), 1)), 0));
                         (Uxn.Read ((Uxn.Var "config"), 1));
                         (Uxn.Read ((Uxn.Var "config"), 0)); (Uxn.Var "config")],
                       []))
                    ))
                  ],
                (Uxn.Stack (
                   [(Uxn.Var "Stop"); (Uxn.Read ((Uxn.Var "config"), 1));
                     (Uxn.Read ((Uxn.Var "config"), 0)); (Uxn.Var "config")],
                   []))
                )),
             (Uxn.Stack ([(Uxn.Var "%ret")], [])))),
          (Uxn.Stack ([(Uxn.Var "%ret")], []))));
       (Uxn.Fun ("step<Apply><FnSub>",
          (Uxn.Stack ([(Uxn.Read ((Uxn.Var "config"), 0)); (Uxn.Var "config")],
             [])),
          (Uxn.Case (
             (Uxn.Stack (
                [(Uxn.Read ((Uxn.Var "config"), 0)); (Uxn.Var "config")],
                [])),
             (Uxn.Read ((Uxn.Read ((Uxn.Var "config"), 0)), 0)),
             [("Int", "step<Apply><FnSub><Int>",
               (Uxn.Stack (
                  [(Uxn.Read ((Uxn.Read ((Uxn.Var "config"), 0)), 0));
                    (Uxn.Read ((Uxn.Var "config"), 0)); (Uxn.Var "config")],
                  [])))
               ]
             )),
          (Uxn.Stack (
             [(Uxn.Read ((Uxn.Read ((Uxn.Var "config"), 0)), 0));
               (Uxn.Read ((Uxn.Var "config"), 0)); (Uxn.Var "config")],
             []))
          ));
       (Uxn.Fun ("step<Apply><FnSub><Int>",
          (Uxn.Stack (
             [(Uxn.Read ((Uxn.Read ((Uxn.Var "config"), 0)), 0));
               (Uxn.Read ((Uxn.Var "config"), 0)); (Uxn.Var "config")],
             [])),
          (Uxn.Case (
             (Uxn.Stack (
                [(Uxn.Read ((Uxn.Read ((Uxn.Var "config"), 0)), 0));
                  (Uxn.Read ((Uxn.Var "config"), 0)); (Uxn.Var "config")],
                [])),
             (Uxn.Read ((Uxn.Var "config"), 1)),
             [("Int", "step<Apply><FnSub><Int><Int>",
               (Uxn.Stack (
                  [(Uxn.Read ((Uxn.Var "config"), 1));
                    (Uxn.Read ((Uxn.Read ((Uxn.Var "config"), 0)), 0));
                    (Uxn.Read ((Uxn.Var "config"), 0)); (Uxn.Var "config")],
                  [])))
               ]
             )),
          (Uxn.Stack (
             [(Uxn.Read ((Uxn.Var "config"), 1));
               (Uxn.Read ((Uxn.Read ((Uxn.Var "config"), 0)), 0));
               (Uxn.Read ((Uxn.Var "config"), 0)); (Uxn.Var "config")],
             []))
          ));
       (Uxn.Fun ("step<Apply><FnSub><Int><Int>",
          (Uxn.Stack (
             [(Uxn.Read ((Uxn.Var "config"), 1));
               (Uxn.Read ((Uxn.Read ((Uxn.Var "config"), 0)), 0));
               (Uxn.Read ((Uxn.Var "config"), 0)); (Uxn.Var "config")],
             [])),
          (Uxn.Ret (
             (Uxn.Stack (
                [(Uxn.Read ((Uxn.Var "config"), 1));
                  (Uxn.Read ((Uxn.Read ((Uxn.Var "config"), 0)), 0));
                  (Uxn.Read ((Uxn.Var "config"), 0)); (Uxn.Var "config")],
                [])),
             (Uxn.Obj (
                (Uxn.Stack (
                   [(Uxn.Var "Int");
                     (Uxn.Read ((Uxn.Read ((Uxn.Var "config"), 0)), 1));
                     (Uxn.Read ((Uxn.Var "config"), 1));
                     (Uxn.Read ((Uxn.Read ((Uxn.Var "config"), 0)), 0));
                     (Uxn.Read ((Uxn.Var "config"), 0)); (Uxn.Var "config")],
                   [])),
                "Eval",
                [(Uxn.Obj (
                    (Uxn.Stack (
                       [(Uxn.Var "SUB2");
                         (Uxn.Read ((Uxn.Read ((Uxn.Var "config"), 0)), 1));
                         (Uxn.Read ((Uxn.Var "config"), 1));
                         (Uxn.Read ((Uxn.Read ((Uxn.Var "config"), 0)), 0));
                         (Uxn.Read ((Uxn.Var "config"), 0)); (Uxn.Var "config")],
                       [])),
                    "Int",
                    [(Uxn.UxnCall (
                        (Uxn.Stack (
                           [(Uxn.Read ((Uxn.Read ((Uxn.Var "config"), 0)), 1));
                             (Uxn.Read ((Uxn.Var "config"), 1));
                             (Uxn.Read ((Uxn.Read ((Uxn.Var "config"), 0)), 0));
                             (Uxn.Read ((Uxn.Var "config"), 0));
                             (Uxn.Var "config")],
                           [])),
                        "SUB2",
                        [(Uxn.Value (
                            (Uxn.Stack (
                               [(Uxn.Read ((Uxn.Read ((Uxn.Var "config"), 0)), 1
                                   ));
                                 (Uxn.Read ((Uxn.Var "config"), 1));
                                 (Uxn.Read ((Uxn.Read ((Uxn.Var "config"), 0)), 0
                                    ));
                                 (Uxn.Read ((Uxn.Var "config"), 0));
                                 (Uxn.Var "config")],
                               [])),
                            (Uxn.Read (
                               (Uxn.Read ((Uxn.Read ((Uxn.Var "config"), 0)), 0)),
                               0)),
                            (Uxn.Stack (
                               [(Uxn.Read (
                                   (Uxn.Read ((Uxn.Read ((Uxn.Var "config"), 0)),
                                      0)),
                                   0));
                                 (Uxn.Read ((Uxn.Read ((Uxn.Var "config"), 0)), 1
                                    ));
                                 (Uxn.Read ((Uxn.Var "config"), 1));
                                 (Uxn.Read ((Uxn.Read ((Uxn.Var "config"), 0)), 0
                                    ));
                                 (Uxn.Read ((Uxn.Var "config"), 0));
                                 (Uxn.Var "config")],
                               []))
                            ));
                          (Uxn.Value (
                             (Uxn.Stack (
                                [(Uxn.Read (
                                    (Uxn.Read (
                                       (Uxn.Read ((Uxn.Var "config"), 0)), 0)),
                                    0));
                                  (Uxn.Read ((Uxn.Read ((Uxn.Var "config"), 0)),
                                     1));
                                  (Uxn.Read ((Uxn.Var "config"), 1));
                                  (Uxn.Read ((Uxn.Read ((Uxn.Var "config"), 0)),
                                     0));
                                  (Uxn.Read ((Uxn.Var "config"), 0));
                                  (Uxn.Var "config")],
                                [])),
                             (Uxn.Read ((Uxn.Read ((Uxn.Var "config"), 1)), 0)),
                             (Uxn.Stack (
                                [(Uxn.Read ((Uxn.Read ((Uxn.Var "config"), 1)), 0
                                    ));
                                  (Uxn.Read (
                                     (Uxn.Read (
                                        (Uxn.Read ((Uxn.Var "config"), 0)), 0)),
                                     0));
                                  (Uxn.Read ((Uxn.Read ((Uxn.Var "config"), 0)),
                                     1));
                                  (Uxn.Read ((Uxn.Var "config"), 1));
                                  (Uxn.Read ((Uxn.Read ((Uxn.Var "config"), 0)),
                                     0));
                                  (Uxn.Read ((Uxn.Var "config"), 0));
                                  (Uxn.Var "config")],
                                []))
                             ))
                          ],
                        (Uxn.Stack (
                           [(Uxn.Var "SUB2");
                             (Uxn.Read ((Uxn.Read ((Uxn.Var "config"), 0)), 1));
                             (Uxn.Read ((Uxn.Var "config"), 1));
                             (Uxn.Read ((Uxn.Read ((Uxn.Var "config"), 0)), 0));
                             (Uxn.Read ((Uxn.Var "config"), 0));
                             (Uxn.Var "config")],
                           []))
                        ))
                      ],
                    (Uxn.Stack (
                       [(Uxn.Var "Int");
                         (Uxn.Read ((Uxn.Read ((Uxn.Var "config"), 0)), 1));
                         (Uxn.Read ((Uxn.Var "config"), 1));
                         (Uxn.Read ((Uxn.Read ((Uxn.Var "config"), 0)), 0));
                         (Uxn.Read ((Uxn.Var "config"), 0)); (Uxn.Var "config")],
                       []))
                    ));
                  (Uxn.Value (
                     (Uxn.Stack (
                        [(Uxn.Read ((Uxn.Var "config"), 1));
                          (Uxn.Read ((Uxn.Read ((Uxn.Var "config"), 0)), 0));
                          (Uxn.Read ((Uxn.Var "config"), 0)); (Uxn.Var "config")],
                        [])),
                     (Uxn.Read ((Uxn.Read ((Uxn.Var "config"), 0)), 1)),
                     (Uxn.Stack (
                        [(Uxn.Read ((Uxn.Read ((Uxn.Var "config"), 0)), 1));
                          (Uxn.Read ((Uxn.Var "config"), 1));
                          (Uxn.Read ((Uxn.Read ((Uxn.Var "config"), 0)), 0));
                          (Uxn.Read ((Uxn.Var "config"), 0)); (Uxn.Var "config")],
                        []))
                     ))
                  ],
                (Uxn.Stack (
                   [(Uxn.Var "Eval"); (Uxn.Read ((Uxn.Var "config"), 1));
                     (Uxn.Read ((Uxn.Read ((Uxn.Var "config"), 0)), 0));
                     (Uxn.Read ((Uxn.Var "config"), 0)); (Uxn.Var "config")],
                   []))
                )),
             (Uxn.Stack ([(Uxn.Var "%ret")], [])))),
          (Uxn.Stack ([(Uxn.Var "%ret")], []))));
       (Uxn.Fun ("step<Apply><FnAdd>",
          (Uxn.Stack ([(Uxn.Read ((Uxn.Var "config"), 0)); (Uxn.Var "config")],
             [])),
          (Uxn.Case (
             (Uxn.Stack (
                [(Uxn.Read ((Uxn.Var "config"), 0)); (Uxn.Var "config")],
                [])),
             (Uxn.Read ((Uxn.Read ((Uxn.Var "config"), 0)), 0)),
             [("Int", "step<Apply><FnAdd><Int>",
               (Uxn.Stack (
                  [(Uxn.Read ((Uxn.Read ((Uxn.Var "config"), 0)), 0));
                    (Uxn.Read ((Uxn.Var "config"), 0)); (Uxn.Var "config")],
                  [])))
               ]
             )),
          (Uxn.Stack (
             [(Uxn.Read ((Uxn.Read ((Uxn.Var "config"), 0)), 0));
               (Uxn.Read ((Uxn.Var "config"), 0)); (Uxn.Var "config")],
             []))
          ));
       (Uxn.Fun ("step<Apply><FnAdd><Int>",
          (Uxn.Stack (
             [(Uxn.Read ((Uxn.Read ((Uxn.Var "config"), 0)), 0));
               (Uxn.Read ((Uxn.Var "config"), 0)); (Uxn.Var "config")],
             [])),
          (Uxn.Case (
             (Uxn.Stack (
                [(Uxn.Read ((Uxn.Read ((Uxn.Var "config"), 0)), 0));
                  (Uxn.Read ((Uxn.Var "config"), 0)); (Uxn.Var "config")],
                [])),
             (Uxn.Read ((Uxn.Var "config"), 1)),
             [("Int", "step<Apply><FnAdd><Int><Int>",
               (Uxn.Stack (
                  [(Uxn.Read ((Uxn.Var "config"), 1));
                    (Uxn.Read ((Uxn.Read ((Uxn.Var "config"), 0)), 0));
                    (Uxn.Read ((Uxn.Var "config"), 0)); (Uxn.Var "config")],
                  [])))
               ]
             )),
          (Uxn.Stack (
             [(Uxn.Read ((Uxn.Var "config"), 1));
               (Uxn.Read ((Uxn.Read ((Uxn.Var "config"), 0)), 0));
               (Uxn.Read ((Uxn.Var "config"), 0)); (Uxn.Var "config")],
             []))
          ));
       (Uxn.Fun ("step<Apply><FnAdd><Int><Int>",
          (Uxn.Stack (
             [(Uxn.Read ((Uxn.Var "config"), 1));
               (Uxn.Read ((Uxn.Read ((Uxn.Var "config"), 0)), 0));
               (Uxn.Read ((Uxn.Var "config"), 0)); (Uxn.Var "config")],
             [])),
          (Uxn.Ret (
             (Uxn.Stack (
                [(Uxn.Read ((Uxn.Var "config"), 1));
                  (Uxn.Read ((Uxn.Read ((Uxn.Var "config"), 0)), 0));
                  (Uxn.Read ((Uxn.Var "config"), 0)); (Uxn.Var "config")],
                [])),
             (Uxn.Obj (
                (Uxn.Stack (
                   [(Uxn.Var "Int");
                     (Uxn.Read ((Uxn.Read ((Uxn.Var "config"), 0)), 1));
                     (Uxn.Read ((Uxn.Var "config"), 1));
                     (Uxn.Read ((Uxn.Read ((Uxn.Var "config"), 0)), 0));
                     (Uxn.Read ((Uxn.Var "config"), 0)); (Uxn.Var "config")],
                   [])),
                "Eval",
                [(Uxn.Obj (
                    (Uxn.Stack (
                       [(Uxn.Var "ADD2");
                         (Uxn.Read ((Uxn.Read ((Uxn.Var "config"), 0)), 1));
                         (Uxn.Read ((Uxn.Var "config"), 1));
                         (Uxn.Read ((Uxn.Read ((Uxn.Var "config"), 0)), 0));
                         (Uxn.Read ((Uxn.Var "config"), 0)); (Uxn.Var "config")],
                       [])),
                    "Int",
                    [(Uxn.UxnCall (
                        (Uxn.Stack (
                           [(Uxn.Read ((Uxn.Read ((Uxn.Var "config"), 0)), 1));
                             (Uxn.Read ((Uxn.Var "config"), 1));
                             (Uxn.Read ((Uxn.Read ((Uxn.Var "config"), 0)), 0));
                             (Uxn.Read ((Uxn.Var "config"), 0));
                             (Uxn.Var "config")],
                           [])),
                        "ADD2",
                        [(Uxn.Value (
                            (Uxn.Stack (
                               [(Uxn.Read ((Uxn.Read ((Uxn.Var "config"), 0)), 1
                                   ));
                                 (Uxn.Read ((Uxn.Var "config"), 1));
                                 (Uxn.Read ((Uxn.Read ((Uxn.Var "config"), 0)), 0
                                    ));
                                 (Uxn.Read ((Uxn.Var "config"), 0));
                                 (Uxn.Var "config")],
                               [])),
                            (Uxn.Read (
                               (Uxn.Read ((Uxn.Read ((Uxn.Var "config"), 0)), 0)),
                               0)),
                            (Uxn.Stack (
                               [(Uxn.Read (
                                   (Uxn.Read ((Uxn.Read ((Uxn.Var "config"), 0)),
                                      0)),
                                   0));
                                 (Uxn.Read ((Uxn.Read ((Uxn.Var "config"), 0)), 1
                                    ));
                                 (Uxn.Read ((Uxn.Var "config"), 1));
                                 (Uxn.Read ((Uxn.Read ((Uxn.Var "config"), 0)), 0
                                    ));
                                 (Uxn.Read ((Uxn.Var "config"), 0));
                                 (Uxn.Var "config")],
                               []))
                            ));
                          (Uxn.Value (
                             (Uxn.Stack (
                                [(Uxn.Read (
                                    (Uxn.Read (
                                       (Uxn.Read ((Uxn.Var "config"), 0)), 0)),
                                    0));
                                  (Uxn.Read ((Uxn.Read ((Uxn.Var "config"), 0)),
                                     1));
                                  (Uxn.Read ((Uxn.Var "config"), 1));
                                  (Uxn.Read ((Uxn.Read ((Uxn.Var "config"), 0)),
                                     0));
                                  (Uxn.Read ((Uxn.Var "config"), 0));
                                  (Uxn.Var "config")],
                                [])),
                             (Uxn.Read ((Uxn.Read ((Uxn.Var "config"), 1)), 0)),
                             (Uxn.Stack (
                                [(Uxn.Read ((Uxn.Read ((Uxn.Var "config"), 1)), 0
                                    ));
                                  (Uxn.Read (
                                     (Uxn.Read (
                                        (Uxn.Read ((Uxn.Var "config"), 0)), 0)),
                                     0));
                                  (Uxn.Read ((Uxn.Read ((Uxn.Var "config"), 0)),
                                     1));
                                  (Uxn.Read ((Uxn.Var "config"), 1));
                                  (Uxn.Read ((Uxn.Read ((Uxn.Var "config"), 0)),
                                     0));
                                  (Uxn.Read ((Uxn.Var "config"), 0));
                                  (Uxn.Var "config")],
                                []))
                             ))
                          ],
                        (Uxn.Stack (
                           [(Uxn.Var "ADD2");
                             (Uxn.Read ((Uxn.Read ((Uxn.Var "config"), 0)), 1));
                             (Uxn.Read ((Uxn.Var "config"), 1));
                             (Uxn.Read ((Uxn.Read ((Uxn.Var "config"), 0)), 0));
                             (Uxn.Read ((Uxn.Var "config"), 0));
                             (Uxn.Var "config")],
                           []))
                        ))
                      ],
                    (Uxn.Stack (
                       [(Uxn.Var "Int");
                         (Uxn.Read ((Uxn.Read ((Uxn.Var "config"), 0)), 1));
                         (Uxn.Read ((Uxn.Var "config"), 1));
                         (Uxn.Read ((Uxn.Read ((Uxn.Var "config"), 0)), 0));
                         (Uxn.Read ((Uxn.Var "config"), 0)); (Uxn.Var "config")],
                       []))
                    ));
                  (Uxn.Value (
                     (Uxn.Stack (
                        [(Uxn.Read ((Uxn.Var "config"), 1));
                          (Uxn.Read ((Uxn.Read ((Uxn.Var "config"), 0)), 0));
                          (Uxn.Read ((Uxn.Var "config"), 0)); (Uxn.Var "config")],
                        [])),
                     (Uxn.Read ((Uxn.Read ((Uxn.Var "config"), 0)), 1)),
                     (Uxn.Stack (
                        [(Uxn.Read ((Uxn.Read ((Uxn.Var "config"), 0)), 1));
                          (Uxn.Read ((Uxn.Var "config"), 1));
                          (Uxn.Read ((Uxn.Read ((Uxn.Var "config"), 0)), 0));
                          (Uxn.Read ((Uxn.Var "config"), 0)); (Uxn.Var "config")],
                        []))
                     ))
                  ],
                (Uxn.Stack (
                   [(Uxn.Var "Eval"); (Uxn.Read ((Uxn.Var "config"), 1));
                     (Uxn.Read ((Uxn.Read ((Uxn.Var "config"), 0)), 0));
                     (Uxn.Read ((Uxn.Var "config"), 0)); (Uxn.Var "config")],
                   []))
                )),
             (Uxn.Stack ([(Uxn.Var "%ret")], [])))),
          (Uxn.Stack ([(Uxn.Var "%ret")], []))));
       (Uxn.Fun ("step<Apply><ArSub>",
          (Uxn.Stack ([(Uxn.Read ((Uxn.Var "config"), 0)); (Uxn.Var "config")],
             [])),
          (Uxn.Case (
             (Uxn.Stack (
                [(Uxn.Read ((Uxn.Var "config"), 0)); (Uxn.Var "config")],
                [])),
             (Uxn.Read ((Uxn.Var "config"), 1)),
             [("Int", "step<Apply><ArSub><Int>",
               (Uxn.Stack (
                  [(Uxn.Read ((Uxn.Var "config"), 1));
                    (Uxn.Read ((Uxn.Var "config"), 0)); (Uxn.Var "config")],
                  [])))
               ]
             )),
          (Uxn.Stack (
             [(Uxn.Read ((Uxn.Var "config"), 1));
               (Uxn.Read ((Uxn.Var "config"), 0)); (Uxn.Var "config")],
             []))
          ));
       (Uxn.Fun ("step<Apply><ArSub><Int>",
          (Uxn.Stack (
             [(Uxn.Read ((Uxn.Var "config"), 1));
               (Uxn.Read ((Uxn.Var "config"), 0)); (Uxn.Var "config")],
             [])),
          (Uxn.Ret (
             (Uxn.Stack (
                [(Uxn.Read ((Uxn.Var "config"), 1));
                  (Uxn.Read ((Uxn.Var "config"), 0)); (Uxn.Var "config")],
                [])),
             (Uxn.Obj (
                (Uxn.Stack (
                   [(Uxn.Read ((Uxn.Read ((Uxn.Var "config"), 0)), 0));
                     (Uxn.Var "FnSub"); (Uxn.Read ((Uxn.Var "config"), 1));
                     (Uxn.Read ((Uxn.Var "config"), 0)); (Uxn.Var "config")],
                   [])),
                "Eval",
                [(Uxn.Value (
                    (Uxn.Stack (
                       [(Uxn.Var "FnSub"); (Uxn.Read ((Uxn.Var "config"), 1));
                         (Uxn.Read ((Uxn.Var "config"), 0)); (Uxn.Var "config")],
                       [])),
                    (Uxn.Read ((Uxn.Read ((Uxn.Var "config"), 0)), 0)),
                    (Uxn.Stack (
                       [(Uxn.Read ((Uxn.Read ((Uxn.Var "config"), 0)), 0));
                         (Uxn.Var "FnSub"); (Uxn.Read ((Uxn.Var "config"), 1));
                         (Uxn.Read ((Uxn.Var "config"), 0)); (Uxn.Var "config")],
                       []))
                    ));
                  (Uxn.Obj (
                     (Uxn.Stack (
                        [(Uxn.Var "Int");
                          (Uxn.Read ((Uxn.Read ((Uxn.Var "config"), 0)), 1));
                          (Uxn.Read ((Uxn.Var "config"), 1));
                          (Uxn.Read ((Uxn.Var "config"), 0)); (Uxn.Var "config")],
                        [])),
                     "FnSub",
                     [(Uxn.Obj (
                         (Uxn.Stack (
                            [(Uxn.Read ((Uxn.Read ((Uxn.Var "config"), 1)), 0));
                              (Uxn.Read ((Uxn.Read ((Uxn.Var "config"), 0)), 1));
                              (Uxn.Read ((Uxn.Var "config"), 1));
                              (Uxn.Read ((Uxn.Var "config"), 0));
                              (Uxn.Var "config")],
                            [])),
                         "Int",
                         [(Uxn.Value (
                             (Uxn.Stack (
                                [(Uxn.Read ((Uxn.Read ((Uxn.Var "config"), 0)), 1
                                    ));
                                  (Uxn.Read ((Uxn.Var "config"), 1));
                                  (Uxn.Read ((Uxn.Var "config"), 0));
                                  (Uxn.Var "config")],
                                [])),
                             (Uxn.Read ((Uxn.Read ((Uxn.Var "config"), 1)), 0)),
                             (Uxn.Stack (
                                [(Uxn.Read ((Uxn.Read ((Uxn.Var "config"), 1)), 0
                                    ));
                                  (Uxn.Read ((Uxn.Read ((Uxn.Var "config"), 0)),
                                     1));
                                  (Uxn.Read ((Uxn.Var "config"), 1));
                                  (Uxn.Read ((Uxn.Var "config"), 0));
                                  (Uxn.Var "config")],
                                []))
                             ))
                           ],
                         (Uxn.Stack (
                            [(Uxn.Var "Int");
                              (Uxn.Read ((Uxn.Read ((Uxn.Var "config"), 0)), 1));
                              (Uxn.Read ((Uxn.Var "config"), 1));
                              (Uxn.Read ((Uxn.Var "config"), 0));
                              (Uxn.Var "config")],
                            []))
                         ));
                       (Uxn.Value (
                          (Uxn.Stack (
                             [(Uxn.Read ((Uxn.Var "config"), 1));
                               (Uxn.Read ((Uxn.Var "config"), 0));
                               (Uxn.Var "config")],
                             [])),
                          (Uxn.Read ((Uxn.Read ((Uxn.Var "config"), 0)), 1)),
                          (Uxn.Stack (
                             [(Uxn.Read ((Uxn.Read ((Uxn.Var "config"), 0)), 1));
                               (Uxn.Read ((Uxn.Var "config"), 1));
                               (Uxn.Read ((Uxn.Var "config"), 0));
                               (Uxn.Var "config")],
                             []))
                          ))
                       ],
                     (Uxn.Stack (
                        [(Uxn.Var "FnSub"); (Uxn.Read ((Uxn.Var "config"), 1));
                          (Uxn.Read ((Uxn.Var "config"), 0)); (Uxn.Var "config")],
                        []))
                     ))
                  ],
                (Uxn.Stack (
                   [(Uxn.Var "Eval"); (Uxn.Read ((Uxn.Var "config"), 1));
                     (Uxn.Read ((Uxn.Var "config"), 0)); (Uxn.Var "config")],
                   []))
                )),
             (Uxn.Stack ([(Uxn.Var "%ret")], [])))),
          (Uxn.Stack ([(Uxn.Var "%ret")], []))));
       (Uxn.Fun ("step<Apply><ArAdd>",
          (Uxn.Stack ([(Uxn.Read ((Uxn.Var "config"), 0)); (Uxn.Var "config")],
             [])),
          (Uxn.Case (
             (Uxn.Stack (
                [(Uxn.Read ((Uxn.Var "config"), 0)); (Uxn.Var "config")],
                [])),
             (Uxn.Read ((Uxn.Var "config"), 1)),
             [("Int", "step<Apply><ArAdd><Int>",
               (Uxn.Stack (
                  [(Uxn.Read ((Uxn.Var "config"), 1));
                    (Uxn.Read ((Uxn.Var "config"), 0)); (Uxn.Var "config")],
                  [])))
               ]
             )),
          (Uxn.Stack (
             [(Uxn.Read ((Uxn.Var "config"), 1));
               (Uxn.Read ((Uxn.Var "config"), 0)); (Uxn.Var "config")],
             []))
          ));
       (Uxn.Fun ("step<Apply><ArAdd><Int>",
          (Uxn.Stack (
             [(Uxn.Read ((Uxn.Var "config"), 1));
               (Uxn.Read ((Uxn.Var "config"), 0)); (Uxn.Var "config")],
             [])),
          (Uxn.Ret (
             (Uxn.Stack (
                [(Uxn.Read ((Uxn.Var "config"), 1));
                  (Uxn.Read ((Uxn.Var "config"), 0)); (Uxn.Var "config")],
                [])),
             (Uxn.Obj (
                (Uxn.Stack (
                   [(Uxn.Read ((Uxn.Read ((Uxn.Var "config"), 0)), 0));
                     (Uxn.Var "FnAdd"); (Uxn.Read ((Uxn.Var "config"), 1));
                     (Uxn.Read ((Uxn.Var "config"), 0)); (Uxn.Var "config")],
                   [])),
                "Eval",
                [(Uxn.Value (
                    (Uxn.Stack (
                       [(Uxn.Var "FnAdd"); (Uxn.Read ((Uxn.Var "config"), 1));
                         (Uxn.Read ((Uxn.Var "config"), 0)); (Uxn.Var "config")],
                       [])),
                    (Uxn.Read ((Uxn.Read ((Uxn.Var "config"), 0)), 0)),
                    (Uxn.Stack (
                       [(Uxn.Read ((Uxn.Read ((Uxn.Var "config"), 0)), 0));
                         (Uxn.Var "FnAdd"); (Uxn.Read ((Uxn.Var "config"), 1));
                         (Uxn.Read ((Uxn.Var "config"), 0)); (Uxn.Var "config")],
                       []))
                    ));
                  (Uxn.Obj (
                     (Uxn.Stack (
                        [(Uxn.Var "Int");
                          (Uxn.Read ((Uxn.Read ((Uxn.Var "config"), 0)), 1));
                          (Uxn.Read ((Uxn.Var "config"), 1));
                          (Uxn.Read ((Uxn.Var "config"), 0)); (Uxn.Var "config")],
                        [])),
                     "FnAdd",
                     [(Uxn.Obj (
                         (Uxn.Stack (
                            [(Uxn.Read ((Uxn.Read ((Uxn.Var "config"), 1)), 0));
                              (Uxn.Read ((Uxn.Read ((Uxn.Var "config"), 0)), 1));
                              (Uxn.Read ((Uxn.Var "config"), 1));
                              (Uxn.Read ((Uxn.Var "config"), 0));
                              (Uxn.Var "config")],
                            [])),
                         "Int",
                         [(Uxn.Value (
                             (Uxn.Stack (
                                [(Uxn.Read ((Uxn.Read ((Uxn.Var "config"), 0)), 1
                                    ));
                                  (Uxn.Read ((Uxn.Var "config"), 1));
                                  (Uxn.Read ((Uxn.Var "config"), 0));
                                  (Uxn.Var "config")],
                                [])),
                             (Uxn.Read ((Uxn.Read ((Uxn.Var "config"), 1)), 0)),
                             (Uxn.Stack (
                                [(Uxn.Read ((Uxn.Read ((Uxn.Var "config"), 1)), 0
                                    ));
                                  (Uxn.Read ((Uxn.Read ((Uxn.Var "config"), 0)),
                                     1));
                                  (Uxn.Read ((Uxn.Var "config"), 1));
                                  (Uxn.Read ((Uxn.Var "config"), 0));
                                  (Uxn.Var "config")],
                                []))
                             ))
                           ],
                         (Uxn.Stack (
                            [(Uxn.Var "Int");
                              (Uxn.Read ((Uxn.Read ((Uxn.Var "config"), 0)), 1));
                              (Uxn.Read ((Uxn.Var "config"), 1));
                              (Uxn.Read ((Uxn.Var "config"), 0));
                              (Uxn.Var "config")],
                            []))
                         ));
                       (Uxn.Value (
                          (Uxn.Stack (
                             [(Uxn.Read ((Uxn.Var "config"), 1));
                               (Uxn.Read ((Uxn.Var "config"), 0));
                               (Uxn.Var "config")],
                             [])),
                          (Uxn.Read ((Uxn.Read ((Uxn.Var "config"), 0)), 1)),
                          (Uxn.Stack (
                             [(Uxn.Read ((Uxn.Read ((Uxn.Var "config"), 0)), 1));
                               (Uxn.Read ((Uxn.Var "config"), 1));
                               (Uxn.Read ((Uxn.Var "config"), 0));
                               (Uxn.Var "config")],
                             []))
                          ))
                       ],
                     (Uxn.Stack (
                        [(Uxn.Var "FnAdd"); (Uxn.Read ((Uxn.Var "config"), 1));
                          (Uxn.Read ((Uxn.Var "config"), 0)); (Uxn.Var "config")],
                        []))
                     ))
                  ],
                (Uxn.Stack (
                   [(Uxn.Var "Eval"); (Uxn.Read ((Uxn.Var "config"), 1));
                     (Uxn.Read ((Uxn.Var "config"), 0)); (Uxn.Var "config")],
                   []))
                )),
             (Uxn.Stack ([(Uxn.Var "%ret")], [])))),
          (Uxn.Stack ([(Uxn.Var "%ret")], []))))
       ])
    |}]
;;

let%expect_test "compile" =
  m
  |> AstNormalize.normalize
  |> IrFromAstNormalized.translate_to_ir
  |> UxnFromIr.from_program
  |> Compile.compile Format.std_formatter;
  [%expect {|
    @step   ( ws: config  --- rs:  ) ( ws: config  --- rs:  )
         ( ws: config config  --- rs:  )
         ( ws: config  --- rs:  )
      LDA2k Apply EQU2 ;step<Apply> JCN2  ( ws: config  --- rs:  )
      LDA2k Eval EQU2 ;step<Eval> JCN2  ( ws: config  --- rs:  )
      LDA2k Main EQU2 ;step<Main> JCN2  ( ws: config  --- rs:  )
      LDA2k Stop EQU2 ;step<Stop> JCN2  ( ws: config  --- rs:  )
      #0000 JSR2r

    @step<Stop>   ( ws: config  --- rs:  ) ( ws: %ret  --- rs:  )
      DUP2   ( ws: config config  --- rs:  )
       #0002 ADD2 LDA2   ( ws: config  --- rs:  ) ( ws: config[0] config  --- rs:  )
       Stop ;push_obj JSR2   ( ws: config[0] config  --- rs:  ) ( ws: Stop config  --- rs:  )
       STH2 POP2 STH2r
       JMP2r ( ws: config  --- rs:  ) ( ws: %ret  --- rs:  )

    @step<Main>   ( ws: config  --- rs:  ) ( ws: %ret  --- rs:  )
      Id ;push_obj JSR2   ( ws: config  --- rs:  ) ( ws: Id config  --- rs:  )
       STH2 DUP2 STH2r SWP2   ( ws: config Id config  --- rs:  )
       #0002 ADD2 LDA2   ( ws: Id config  --- rs:  ) ( ws: config[0] Id config  --- rs:  )
       Int ;push_obj JSR2   ( ws: config[0] Id config  --- rs:  ) ( ws: Int Id config  --- rs:  )
       Eval ;push_obj JSR2   ( ws: Int Id config  --- rs:  ) ( ws: Eval config  --- rs:  )
       STH2 POP2 STH2r
       JMP2r ( ws: config  --- rs:  ) ( ws: %ret  --- rs:  )

    @step<Eval>   ( ws: config  --- rs:  ) ( ws: config[0] config  --- rs:  )
      DUP2   ( ws: config config  --- rs:  )
       #0002 ADD2 LDA2   ( ws: config  --- rs:  )
      LDA2k Add EQU2 ;step<Eval><Add> JCN2  ( ws: config[0] config  --- rs:  )
      LDA2k Int EQU2 ;step<Eval><Int> JCN2  ( ws: config[0] config  --- rs:  )
      LDA2k Sub EQU2 ;step<Eval><Sub> JCN2  ( ws: config[0] config  --- rs:  )
      #0000 JSR2r

    @step<Eval><Sub>   ( ws: config[0] config  --- rs:  ) ( ws: %ret  --- rs:  )
      STH2 DUP2 STH2r SWP2   ( ws: config config[0] config  --- rs:  )
       #0004 ADD2 LDA2   ( ws: config[0] config  --- rs:  ) ( ws: config[1] config[0] config  --- rs:  )
       STH2 STH2 DUP2 STH2r SWP2 STH2r SWP2   ( ws: config config[1] config[0] config  --- rs:  )
       #0002 ADD2 LDA2 #0004 ADD2 LDA2   ( ws: config[1] config[0] config  --- rs:  ) ( ws: config[0][1] config[1] config[0] config  --- rs:  )
       ArSub ;push_obj JSR2   ( ws: config[0][1] config[1] config[0] config  --- rs:  ) ( ws: ArSub config[0] config  --- rs:  )
       STH2 STH2 DUP2 STH2r SWP2 STH2r SWP2   ( ws: config ArSub config[0] config  --- rs:  )
       #0002 ADD2 LDA2 #0002 ADD2 LDA2   ( ws: ArSub config[0] config  --- rs:  ) ( ws: config[0][0] ArSub config[0] config  --- rs:  )
       Eval ;push_obj JSR2   ( ws: config[0][0] ArSub config[0] config  --- rs:  ) ( ws: Eval config[0] config  --- rs:  )
       STH2 POP2 POP2 STH2r
       JMP2r ( ws: config[0] config  --- rs:  ) ( ws: %ret  --- rs:  )

    @step<Eval><Int>   ( ws: config[0] config  --- rs:  ) ( ws: %ret  --- rs:  )
      STH2 DUP2 STH2r SWP2   ( ws: config config[0] config  --- rs:  )
       #0002 ADD2 LDA2 #0002 ADD2 LDA2   ( ws: config[0] config  --- rs:  ) ( ws: config[0][0] config[0] config  --- rs:  )
       Int ;push_obj JSR2   ( ws: config[0][0] config[0] config  --- rs:  ) ( ws: Int config[0] config  --- rs:  )
       STH2 STH2 DUP2 STH2r SWP2 STH2r SWP2   ( ws: config Int config[0] config  --- rs:  )
       #0004 ADD2 LDA2   ( ws: Int config[0] config  --- rs:  ) ( ws: config[1] Int config[0] config  --- rs:  )
       Apply ;push_obj JSR2   ( ws: config[1] Int config[0] config  --- rs:  ) ( ws: Apply config[0] config  --- rs:  )
       STH2 POP2 POP2 STH2r
       JMP2r ( ws: config[0] config  --- rs:  ) ( ws: %ret  --- rs:  )

    @step<Eval><Add>   ( ws: config[0] config  --- rs:  ) ( ws: %ret  --- rs:  )
      STH2 DUP2 STH2r SWP2   ( ws: config config[0] config  --- rs:  )
       #0004 ADD2 LDA2   ( ws: config[0] config  --- rs:  ) ( ws: config[1] config[0] config  --- rs:  )
       STH2 STH2 DUP2 STH2r SWP2 STH2r SWP2   ( ws: config config[1] config[0] config  --- rs:  )
       #0002 ADD2 LDA2 #0004 ADD2 LDA2   ( ws: config[1] config[0] config  --- rs:  ) ( ws: config[0][1] config[1] config[0] config  --- rs:  )
       ArAdd ;push_obj JSR2   ( ws: config[0][1] config[1] config[0] config  --- rs:  ) ( ws: ArAdd config[0] config  --- rs:  )
       STH2 STH2 DUP2 STH2r SWP2 STH2r SWP2   ( ws: config ArAdd config[0] config  --- rs:  )
       #0002 ADD2 LDA2 #0002 ADD2 LDA2   ( ws: ArAdd config[0] config  --- rs:  ) ( ws: config[0][0] ArAdd config[0] config  --- rs:  )
       Eval ;push_obj JSR2   ( ws: config[0][0] ArAdd config[0] config  --- rs:  ) ( ws: Eval config[0] config  --- rs:  )
       STH2 POP2 POP2 STH2r
       JMP2r ( ws: config[0] config  --- rs:  ) ( ws: %ret  --- rs:  )

    @step<Apply>   ( ws: config  --- rs:  ) ( ws: config[0] config  --- rs:  )
      DUP2   ( ws: config config  --- rs:  )
       #0002 ADD2 LDA2   ( ws: config  --- rs:  )
      LDA2k ArAdd EQU2 ;step<Apply><ArAdd> JCN2  ( ws: config[0] config  --- rs:  )
      LDA2k ArSub EQU2 ;step<Apply><ArSub> JCN2  ( ws: config[0] config  --- rs:  )
      LDA2k FnAdd EQU2 ;step<Apply><FnAdd> JCN2  ( ws: config[0] config  --- rs:  )
      LDA2k FnSub EQU2 ;step<Apply><FnSub> JCN2  ( ws: config[0] config  --- rs:  )
      LDA2k Id EQU2 ;step<Apply><Id> JCN2  ( ws: config[0] config  --- rs:  )
      #0000 JSR2r

    @step<Apply><Id>   ( ws: config[0] config  --- rs:  ) ( ws: config[1] config[0] config  --- rs:  )
      STH2 DUP2 STH2r SWP2   ( ws: config config[0] config  --- rs:  )
       #0004 ADD2 LDA2   ( ws: config[0] config  --- rs:  )
      LDA2k Int EQU2 ;step<Apply><Id><Int> JCN2  ( ws: config[1] config[0] config  --- rs:  )
      #0000 JSR2r

    @step<Apply><Id><Int>   ( ws: config[1] config[0] config  --- rs:  ) ( ws: %ret  --- rs:  )
      STH2 STH2 DUP2 STH2r SWP2 STH2r SWP2   ( ws: config config[1] config[0] config  --- rs:  )
       #0004 ADD2 LDA2 #0002 ADD2 LDA2   ( ws: config[1] config[0] config  --- rs:  ) ( ws: config[1][0] config[1] config[0] config  --- rs:  )
       Stop ;push_obj JSR2   ( ws: config[1][0] config[1] config[0] config  --- rs:  ) ( ws: Stop config[1] config[0] config  --- rs:  )
       STH2 POP2 POP2 POP2 STH2r
       JMP2r ( ws: config[1] config[0] config  --- rs:  ) ( ws: %ret  --- rs:  )

    @step<Apply><FnSub>   ( ws: config[0] config  --- rs:  ) ( ws: config[0][0] config[0] config  --- rs:  )
      STH2 DUP2 STH2r SWP2   ( ws: config config[0] config  --- rs:  )
       #0002 ADD2 LDA2 #0002 ADD2 LDA2   ( ws: config[0] config  --- rs:  )
      LDA2k Int EQU2 ;step<Apply><FnSub><Int> JCN2  ( ws: config[0][0] config[0] config  --- rs:  )
      #0000 JSR2r

    @step<Apply><FnSub><Int>   ( ws: config[0][0] config[0] config  --- rs:  ) ( ws: config[1] config[0][0] config[0] config  --- rs:  )
      STH2 STH2 DUP2 STH2r SWP2 STH2r SWP2   ( ws: config config[0][0] config[0] config  --- rs:  )
       #0004 ADD2 LDA2   ( ws: config[0][0] config[0] config  --- rs:  )
      LDA2k Int EQU2 ;step<Apply><FnSub><Int><Int> JCN2  ( ws: config[1] config[0][0] config[0] config  --- rs:  )
      #0000 JSR2r

    @step<Apply><FnSub><Int><Int>   ( ws: config[1] config[0][0] config[0] config  --- rs:  ) ( ws: %ret  --- rs:  )
      STH2 STH2 STH2 DUP2 STH2r SWP2 STH2r SWP2 STH2r SWP2   ( ws: config config[1] config[0][0] config[0] config  --- rs:  )
       #0002 ADD2 LDA2 #0004 ADD2 LDA2   ( ws: config[1] config[0][0] config[0] config  --- rs:  ) ( ws: config[0][1] config[1] config[0][0] config[0] config  --- rs:  )
       STH2 STH2 STH2 STH2 DUP2 STH2r SWP2 STH2r SWP2 STH2r SWP2 STH2r SWP2   ( ws: config config[0][1] config[1] config[0][0] config[0] config  --- rs:  )
       #0002 ADD2 LDA2 #0002 ADD2 LDA2 #0002 ADD2 LDA2   ( ws: config[0][1] config[1] config[0][0] config[0] config  --- rs:  ) ( ws: config[0][0][0] config[0][1] config[1] config[0][0] config[0] config  --- rs:  )
       STH2 STH2 STH2 STH2 STH2 DUP2 STH2r SWP2 STH2r SWP2 STH2r SWP2 STH2r SWP2 STH2r SWP2   ( ws: config config[0][0][0] config[0][1] config[1] config[0][0] config[0] config  --- rs:  )
       #0004 ADD2 LDA2 #0002 ADD2 LDA2   ( ws: config[0][0][0] config[0][1] config[1] config[0][0] config[0] config  --- rs:  ) ( ws: config[1][0] config[0][0][0] config[0][1] config[1] config[0][0] config[0] config  --- rs:  )
        SUB2   ( ws: config[0][1] config[1] config[0][0] config[0] config  --- rs:  ) ( ws: SUB2 config[0][1] config[1] config[0][0] config[0] config  --- rs:  )
       Int ;push_obj JSR2   ( ws: SUB2 config[0][1] config[1] config[0][0] config[0] config  --- rs:  ) ( ws: Int config[0][1] config[1] config[0][0] config[0] config  --- rs:  )
       Eval ;push_obj JSR2   ( ws: Int config[0][1] config[1] config[0][0] config[0] config  --- rs:  ) ( ws: Eval config[1] config[0][0] config[0] config  --- rs:  )
       STH2 POP2 POP2 POP2 POP2 STH2r
       JMP2r ( ws: config[1] config[0][0] config[0] config  --- rs:  ) ( ws: %ret  --- rs:  )

    @step<Apply><FnAdd>   ( ws: config[0] config  --- rs:  ) ( ws: config[0][0] config[0] config  --- rs:  )
      STH2 DUP2 STH2r SWP2   ( ws: config config[0] config  --- rs:  )
       #0002 ADD2 LDA2 #0002 ADD2 LDA2   ( ws: config[0] config  --- rs:  )
      LDA2k Int EQU2 ;step<Apply><FnAdd><Int> JCN2  ( ws: config[0][0] config[0] config  --- rs:  )
      #0000 JSR2r

    @step<Apply><FnAdd><Int>   ( ws: config[0][0] config[0] config  --- rs:  ) ( ws: config[1] config[0][0] config[0] config  --- rs:  )
      STH2 STH2 DUP2 STH2r SWP2 STH2r SWP2   ( ws: config config[0][0] config[0] config  --- rs:  )
       #0004 ADD2 LDA2   ( ws: config[0][0] config[0] config  --- rs:  )
      LDA2k Int EQU2 ;step<Apply><FnAdd><Int><Int> JCN2  ( ws: config[1] config[0][0] config[0] config  --- rs:  )
      #0000 JSR2r

    @step<Apply><FnAdd><Int><Int>   ( ws: config[1] config[0][0] config[0] config  --- rs:  ) ( ws: %ret  --- rs:  )
      STH2 STH2 STH2 DUP2 STH2r SWP2 STH2r SWP2 STH2r SWP2   ( ws: config config[1] config[0][0] config[0] config  --- rs:  )
       #0002 ADD2 LDA2 #0004 ADD2 LDA2   ( ws: config[1] config[0][0] config[0] config  --- rs:  ) ( ws: config[0][1] config[1] config[0][0] config[0] config  --- rs:  )
       STH2 STH2 STH2 STH2 DUP2 STH2r SWP2 STH2r SWP2 STH2r SWP2 STH2r SWP2   ( ws: config config[0][1] config[1] config[0][0] config[0] config  --- rs:  )
       #0002 ADD2 LDA2 #0002 ADD2 LDA2 #0002 ADD2 LDA2   ( ws: config[0][1] config[1] config[0][0] config[0] config  --- rs:  ) ( ws: config[0][0][0] config[0][1] config[1] config[0][0] config[0] config  --- rs:  )
       STH2 STH2 STH2 STH2 STH2 DUP2 STH2r SWP2 STH2r SWP2 STH2r SWP2 STH2r SWP2 STH2r SWP2   ( ws: config config[0][0][0] config[0][1] config[1] config[0][0] config[0] config  --- rs:  )
       #0004 ADD2 LDA2 #0002 ADD2 LDA2   ( ws: config[0][0][0] config[0][1] config[1] config[0][0] config[0] config  --- rs:  ) ( ws: config[1][0] config[0][0][0] config[0][1] config[1] config[0][0] config[0] config  --- rs:  )
        ADD2   ( ws: config[0][1] config[1] config[0][0] config[0] config  --- rs:  ) ( ws: ADD2 config[0][1] config[1] config[0][0] config[0] config  --- rs:  )
       Int ;push_obj JSR2   ( ws: ADD2 config[0][1] config[1] config[0][0] config[0] config  --- rs:  ) ( ws: Int config[0][1] config[1] config[0][0] config[0] config  --- rs:  )
       Eval ;push_obj JSR2   ( ws: Int config[0][1] config[1] config[0][0] config[0] config  --- rs:  ) ( ws: Eval config[1] config[0][0] config[0] config  --- rs:  )
       STH2 POP2 POP2 POP2 POP2 STH2r
       JMP2r ( ws: config[1] config[0][0] config[0] config  --- rs:  ) ( ws: %ret  --- rs:  )

    @step<Apply><ArSub>   ( ws: config[0] config  --- rs:  ) ( ws: config[1] config[0] config  --- rs:  )
      STH2 DUP2 STH2r SWP2   ( ws: config config[0] config  --- rs:  )
       #0004 ADD2 LDA2   ( ws: config[0] config  --- rs:  )
      LDA2k Int EQU2 ;step<Apply><ArSub><Int> JCN2  ( ws: config[1] config[0] config  --- rs:  )
      #0000 JSR2r

    @step<Apply><ArSub><Int>   ( ws: config[1] config[0] config  --- rs:  ) ( ws: %ret  --- rs:  )
      STH2 STH2 DUP2 STH2r SWP2 STH2r SWP2   ( ws: config config[1] config[0] config  --- rs:  )
       #0002 ADD2 LDA2 #0004 ADD2 LDA2   ( ws: config[1] config[0] config  --- rs:  ) ( ws: config[0][1] config[1] config[0] config  --- rs:  )
       STH2 STH2 STH2 DUP2 STH2r SWP2 STH2r SWP2 STH2r SWP2   ( ws: config config[0][1] config[1] config[0] config  --- rs:  )
       #0004 ADD2 LDA2 #0002 ADD2 LDA2   ( ws: config[0][1] config[1] config[0] config  --- rs:  ) ( ws: config[1][0] config[0][1] config[1] config[0] config  --- rs:  )
       Int ;push_obj JSR2   ( ws: config[1][0] config[0][1] config[1] config[0] config  --- rs:  ) ( ws: Int config[0][1] config[1] config[0] config  --- rs:  )
       FnSub ;push_obj JSR2   ( ws: Int config[0][1] config[1] config[0] config  --- rs:  ) ( ws: FnSub config[1] config[0] config  --- rs:  )
       STH2 STH2 STH2 DUP2 STH2r SWP2 STH2r SWP2 STH2r SWP2   ( ws: config FnSub config[1] config[0] config  --- rs:  )
       #0002 ADD2 LDA2 #0002 ADD2 LDA2   ( ws: FnSub config[1] config[0] config  --- rs:  ) ( ws: config[0][0] FnSub config[1] config[0] config  --- rs:  )
       Eval ;push_obj JSR2   ( ws: config[0][0] FnSub config[1] config[0] config  --- rs:  ) ( ws: Eval config[1] config[0] config  --- rs:  )
       STH2 POP2 POP2 POP2 STH2r
       JMP2r ( ws: config[1] config[0] config  --- rs:  ) ( ws: %ret  --- rs:  )

    @step<Apply><ArAdd>   ( ws: config[0] config  --- rs:  ) ( ws: config[1] config[0] config  --- rs:  )
      STH2 DUP2 STH2r SWP2   ( ws: config config[0] config  --- rs:  )
       #0004 ADD2 LDA2   ( ws: config[0] config  --- rs:  )
      LDA2k Int EQU2 ;step<Apply><ArAdd><Int> JCN2  ( ws: config[1] config[0] config  --- rs:  )
      #0000 JSR2r

    @step<Apply><ArAdd><Int>   ( ws: config[1] config[0] config  --- rs:  ) ( ws: %ret  --- rs:  )
      STH2 STH2 DUP2 STH2r SWP2 STH2r SWP2   ( ws: config config[1] config[0] config  --- rs:  )
       #0002 ADD2 LDA2 #0004 ADD2 LDA2   ( ws: config[1] config[0] config  --- rs:  ) ( ws: config[0][1] config[1] config[0] config  --- rs:  )
       STH2 STH2 STH2 DUP2 STH2r SWP2 STH2r SWP2 STH2r SWP2   ( ws: config config[0][1] config[1] config[0] config  --- rs:  )
       #0004 ADD2 LDA2 #0002 ADD2 LDA2   ( ws: config[0][1] config[1] config[0] config  --- rs:  ) ( ws: config[1][0] config[0][1] config[1] config[0] config  --- rs:  )
       Int ;push_obj JSR2   ( ws: config[1][0] config[0][1] config[1] config[0] config  --- rs:  ) ( ws: Int config[0][1] config[1] config[0] config  --- rs:  )
       FnAdd ;push_obj JSR2   ( ws: Int config[0][1] config[1] config[0] config  --- rs:  ) ( ws: FnAdd config[1] config[0] config  --- rs:  )
       STH2 STH2 STH2 DUP2 STH2r SWP2 STH2r SWP2 STH2r SWP2   ( ws: config FnAdd config[1] config[0] config  --- rs:  )
       #0002 ADD2 LDA2 #0002 ADD2 LDA2   ( ws: FnAdd config[1] config[0] config  --- rs:  ) ( ws: config[0][0] FnAdd config[1] config[0] config  --- rs:  )
       Eval ;push_obj JSR2   ( ws: config[0][0] FnAdd config[1] config[0] config  --- rs:  ) ( ws: Eval config[1] config[0] config  --- rs:  )
       STH2 POP2 POP2 POP2 STH2r
       JMP2r ( ws: config[1] config[0] config  --- rs:  ) ( ws: %ret  --- rs:  )
    |}]
;;
