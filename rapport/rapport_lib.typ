#import "lib.typ": ieee
#import "@preview/diagraph:0.3.4": raw-render
#import "@preview/algo:0.3.6": algo, i, d, comment, code
#import "@preview/bob-draw:0.1.1": render
#import "@preview/curryst:0.6.0": prooftree, rule

// #show regex("while|done|do|if|then|else|endif|;|skip|assert"): set text(purple, weight: "black")

#set page(footer: context [
  #set text(9pt)
  #h(1fr)
  #counter(page).display("1/1", both: true)
])

#let def_counter = counter("def")
#let def(name: none, body) = {
  // let h = context counter(heading).get()
  // let h_ = context counter(heading).display()
  def_counter.step()
  let txt = if name == none [] else [(#name)]
  let nb = context counter(heading).get().at(0)
  block(
    stroke: (left : 1pt),
    inset:(x: 4pt, y: 2pt),
    [*Definition #context [#nb.#def_counter.display()]* #txt
    \ #body]
  )
}
#let th_counter = counter("th")
#let th(name: none, body) = {
  th_counter.step()
  let txt = if name == none [] else [(#name)]
  let nb = context counter(heading).get().at(0)
  block(
    stroke: (left : 1pt),
    inset:(x: 4pt, y: 2pt),
    [*Theorem #context [#nb.#th_counter.display()]* #txt
    \ #body]
  )
}
#let ex_counter = counter("ex")
#let ex(name: none, body) = {
  ex_counter.step()
  let txt = if name == none [] else [(#name)]
  let nb = context counter(heading).get().at(0)
  block(
    stroke: (left : 1pt),
    inset:(x: 4pt, y: 2pt),
    [*Example #context [#nb.#ex_counter.display()]* #txt
    \ #body]
  )
}

#show: ieee.with(
  title: [Compilation de machine abstraite vers Uxn],
  abstract: [
    Alan SCHMITT et Martin ANDRIEUX travaillent sur un projet de traduction de description de sémantique en machine abstraite. Mon travail consistait alors à continuer la transformation en récupérant la machine abstraite et en la compilant vers le langage assembleur Uxn.

    J'ai alors dans un premier temps écrit une petite machine abstraite étant une évaluateur d'expressions arithmétiques simples. J'ai ensuite appris à utiliser le langage assembleur Uxn. J'ai dû coder des fonctions de manipulation de la mémoire, un équivalent de _free_ et _malloc_. Enfin, j'ai traduit à la main la petite machine abstraite en assembleur Uxn.

    Dans un second temps, j'ai coder le compilateur en entier. Il s'appuie sur une chaîne de compilation avec plusieurs représentations internes et transformations sur ces représentations.
  ],
  authors: (
    (
      name: "Timothée MENEUX",
      department: [Stagiaire],
      organization: [Ecole Normale Supérieure],
      location: [Rennes, France],
      email: "timothee.meneux@ens-rennes.fr"
    ),
    (
      name: "Alan SCHMITT",
      department: [Encadrant],
      organization: [Epicure, IRISA-INRIA],
      location: [Rennes, France],
      email: "alan.schmitt@inria.fr"
    ),
    (
      name: "Martin ANDRIEUX",
      department: [Co-encadrant],
      organization: [Epicure, IRISA-INRIA],
      location: [Rennes, France],
      email: "martin.andrieux@inria.fr"
    )
  ),
  paper-size: "a4",
  index-terms: ("Machines Abstraites", "Compilation","Uxn"),
  bibliography: bibliography("refs.bib"),
)
// #set text(size: 11pt)
// Your content goes below.
= Introduction
_*Contexte.*_
Les machines abstraites prennent la forme d'interpréteurs récursifs terminaux et de premier ordre, sous forme d'un système de transitions @fun_cor_am. Elles constituent le format de sémantique le plus précis et le plus bas niveau ; et sont donc particulièrement adaptées pour une implémentation directe en assembleur. Uxn est une architecture de jeu d’instructions minimale (elle ne contient que peu d’instructions) et open source.

_*Objet de recherche.*_
Le projet consiste à développer un compilateur prenant en entrée une description de machine abstraite et produisant en sortie une implémentation de cette machine en assembleur Uxn.

_*Contributions.*_
Le projet se déroulera en deux phases. Une première phase sera l’implémentation à la main d’une machine simple en Uxn. La deuxième phase consistera en la généralisation et l’automatisation de cette transformation, en utilisant nos outils de manipulations de sémantiques @skeletons. Si le temps le permet, la transformation pourra être prouvée correcte (sur papier).

_*Travaux des pairs.*_
Ce projet s’intègre dans des travaux en cours sur la compilation générique de programmes @am_for_prog_lang dont la sémantique est décrite en Skel, un langage haut niveau de description de sémantiques, et pour lequel un générateur de machines abstraites est déjà réalisé.

= Première machine abstraite
#let languageColor1 = blue.darken(20%)
#let languageColor2 = red.darken(20%)
#let coloring(x, color) = {
  set text(color); $#x$
}

#let expr = $e x p r$
#let Int = $coloring(I n t, languageColor1)$
#let Add = $coloring(A d d, languageColor1)$
#let Sub = $coloring(S u b, languageColor1)$
#let cont = $c o n t$
#let Id = $coloring(I d, languageColor2)$
#let Fnadd = $coloring(F n_(A d d), languageColor2)$
#let Aradd = $coloring(A r_(A d d), languageColor2)$
#let Fnsub = $coloring(F n_(S u b), languageColor2)$
#let Arsub = $coloring(A r_(S u b), languageColor2)$

== Syntaxe
On introduit notre machine abstraite cible, un évaluateur d'expressions arithmétiques simples.

#figure(caption: [Syntaxe des expressions])[
  $
    expr ::=& Int(i) & i in ZZ\
    |& Add (expr,expr)\
    |& Sub (expr,expr)
  $
] <expr_syn>

#figure(caption: [Syntaxe des continuations])[
  $
    cont ::=& Id\
    |& Aradd (expr, cont)\
    |& Fnadd (expr, cont)\
    |& Arsub (expr, cont)\
    |& Fnsub (expr, cont)
  $
] <cont_syn>

On notera $cal(L)(expr)$ et $cal(L)(cont)$ comme étant les langages reconnus par les grammaires $expr$ et $cont$.

== Sémantique concrète
Nos expressions admettent une sémantique concrète, celle de l'arithmétique. On évalue ces termes avec la fonction $[|dot|]_(s c) : cal(L)(expr) --> ZZ$ définie comme suit :

#figure(caption: [Sémantique concrète])[
  $
    [|Int(i)|]_(s c) &= i\
    [|Add(e_1, e_2)|]_(s c) &= [|e_1|]_(s c) + [|e_2|]_(s c)\
    [|Sub(e_1, e_2)|]_(s c) &= [|e_1|]_(s c) - [|e_2|]_(s c)

  $
] <sem>

== Sémantique de la machine abstraite
#let eval = $e v a l$
#let Eval(e,k) = $chevron.l #e | #k chevron.r_bold(coloring(e,languageColor1))$
#let apply = $a p p l y$
#let Apply(e,k) = $chevron.l #e | #k chevron.r_bold(coloring(a,languageColor2))$
#let res = $r e s$
#let Res(i) = $chevron.l #i chevron.r_bold(coloring(r, #green.darken(20%)))$
#let fail = $coloring(F a i l, #red.darken(20%))$
#let step = $scripts(-->)^sharp$
#let steps = $scripts(-->)^(sharp*)$

Pour créer notre machine abstraite $cal(M)^sharp$, on la munie de trois états $eval$, $apply$ et $res$ ainsi que d'un système de transition $step$ étant une sémantique à petits pas.

#figure(caption: [États de la machine abstraite])[
  $
    eval &::= Eval(expr, cont) \
    apply &::= Apply(cont, expr) \
    res &::= Res(i) | fail & i in ZZ
  $
] <am_states>

#figure(caption: [Transitions pour les états $eval$])[
  $
    &Eval(Int(i), kappa)        &&step Apply(kappa, i) quad quad quad i in ZZ\
    &Eval(Add(e_1, e_2), kappa) &&step Eval(e_1, Aradd (e_2, kappa))\
    &Eval(Sub(e_1, e_2), kappa) &&step Eval(e_1, Arsub (e_2, kappa))
  $
] <eval_trans>

#figure(caption: [Transitions pour les états $apply$])[
  $
    &Apply(Id, Int(i))                  &&step Res(i)\
    &Apply(Aradd(e, kappa), Int(i))     &&step Eval(e, Fnadd (Int(i), kappa))\
    &Apply(Arsub(e, kappa), Int(i))     &&step Eval(e, Fnadd (Int(i), kappa))
  $$
    &Apply(Fnadd(Int(i_1), kappa), Int(i_2)) step Eval(Int(i_1 + i_2), kappa)\
    &Apply(Fnsub(Int(i_1), kappa), Int(i_2)) step Eval(Int(i_1 - i_2), kappa)
  $
  // $
  //   #prooftree(rule(
  //     $e_1 = Int(i_1)$,
  //     $e_2 = Int(i_2)$,
  //     $Apply(Fnadd(e_1, kappa), e_2) &&step Eval(Int(i_1 + i_2), kappa)$
  //   ))\ \
  //   #prooftree(rule(
  //     $e_1 = Int(i_1)$,
  //     $e_2 = Int(i_2)$,
  //     $Apply(Fnsub(e_1, kappa), e_2) &&step Eval(Int(i_1 - i_2), kappa)$
  //   ))
  // $
] <apply_trans>

En @apply_trans, les opérations "$+$" et "$-$" représentent le vrai calcul opéré par la machine. De plus, les états $res$ n'ont pas de successeur puisqu'ils symbolisent les états finaux de la machine.

On définit alors la clôture réflexive transitive de la relation de transition $step$ par $steps$. Pour deux états $sigma$ et $sigma'$, on a $sigma steps sigma'$ si et seulement si il existe une suite finie d'états $(sigma_i)_(0<=i<=n)$ telle que $sigma_0 = sigma$, $sigma_n = sigma'$ et pour tout $0<=i<n$, $sigma_i step sigma_(i+1)$.

On pose donc la fonction d'évaluation associée à la machine abstraite $[|dot|]^sharp : cal(L)(expr) --> ZZ$ comme étant le résultat de cette dernière sur l'entrée. Explicitement, on a $[|e|]^sharp = i$ si et seulement si $Eval(e, Id) steps Res(i)$.

= Au niveau machine
== Présentation rapide de Uxn
Uxn est un langage assembleur minimaliste qui s'exécute sur une machine (virtuelle) à pile normée Varvara. L'écosystème Uxn/Varvara permet d'exécuter la même application sur divers systèmes. Cet écosystème a par ailleurs été pensé pour opérer sur des systèmes léger, d'où son minimalisme.

En Uxn, on manipule une machine à pile simple ayant deux piles de 256 octets, une de travail et une de retour, ainsi qu'un espace mémoire adressable de $2^16$ octets ($64$ko).

Comme on manipule des piles, pour faire des calculs, on ajoute d'abord les opérandes sur la piles puis l'opérateur, l'_opcode_. Ainsi, en _Uxn_ on écrit un code assembleur en notation polonaise inversée.

En Uxn, on dispose de 32 _opcodes_ standards et 4 _opcodes_ immédiats. Un _opcode_ est un nom dont les trois premiers caractères figurent dans la table des _opcodes_, suivis d'une combinaison de "2", "k" et "r". Chaque _opcode_ possède trois modes possibles, combinables :
- _short_ (2) : manipule des _shorts_ (2 octets) au lieu d'octets,
- _keep_ (k) : ne consomme aucun élément,
- _return_ (r) : manipule la pile de retour.

Par défaut, les _opcodes_ consomment des octets de la pile de travail. Les _opcodes_ immédiats admettent des "runes", permettant de simplifier l'écriture du code. Par exemple, l'_opcode_ immédiats "LIT" qui empile un octet sur la pile peut être remplacé par la rune "\#".

#figure(kind: "ex", supplement: "ex.", caption: [deux calculs de $(1+2) times 9 = 27$ en Uxn, avec en commentaire l'état de la pile de travail.])[
  #set text(9pt)
  #grid(
    column-gutter: 5pt,
    columns: 2,
    algo(comment-prefix: "")[
      LIT 01 #comment[( ws: 01 )]\
      \#02 #comment[( ws: 02 01 )]\
      ADD #comment[( ws: 03 )]\
      \#09 #comment[( ws: 09 03 )]\
      MUL #comment[( ws: 1b )]
    ],
    algo(comment-prefix: "")[
      \#09 #comment[( ws: 09 )]\
      \#01 #comment[( ws: 01 09 )]\
      \#02 #comment[( ws: 02 01 09 )]\
      ADD #comment[( ws: 03 09 )]\
      MUL #comment[( ws: 1b )]
  ]
  )
] <ex_uxn>

Remarquons que dans l'exemple précédent (@ex_uxn), on peut déplacer des lignes sans que cela n'affecte le calcul.

== Représentation machine des objets <repr_obj>
#let tag = $t a g$
#let size = $s i z e$
#let data = $d a t a$
Pour la représentation des objets, j'ai décidé de m'inspirer de la représentation à la _OCaml_. Ainsi, un objet est représenté par un pointeur vers une section mémoire contiguë qui peut être vue comme un triplet $chevron.curly tag | size | data chevron.r.curly$ où :
- $tag$ : indique le numéro du constructeur utilisé pour créer l'objet, codé sur un octet,
- $size$ : indique le nombre de _shorts_ réservés après, codé sur un octet,
- $data$ : est l'espace utilisable en mémoire, un tableau de $size$ shorts.
On remarque alors que la section mémoire réservée par un objet occupe $2times(size+1)$ octets, soit $size+1$ _shorts_. Ainsi, le champs _data_ peut contenir au plus $255$ shorts (car $size <= 255 = 2^8-1$).

Il est intentionnel que les champs $tag$ et $size$ soient codés sur un octet chacun, ils sont contigus en mémoire et l'on ne travaillera qu'avec des _shorts_, ce qui est le cas lors de la concaténation de ces deux champs. Cette concaténation est alors appelée le _header_.

On appelle alors une case mémoire l'espace mémoire nécessaire pour stocker un _short_ aligné.

== Gestion mémoire
Afin de gérer la mémoire (de $64$ko), on va équiper notre machine d'un équivalent de _malloc_ et de _free_.

Le principe est simple. L'espace addressable est divisé en trois, une section _text_, une section _code_ et une section _raw_. Les sections _text_ et _code_ contiennent le code assembleur chargé et les différentes variables d'environnement (qui ne peuvent pas avoir de valeur par défaut dans la section _text_).

Il nous reste alors toute la section _raw_ qui peut être utilisée pour stocker dynamiquement des objets.

Lors d'un _malloc_, on va chercher la première case ayant un _tag_ nul (_ie_ le _tag_ vaut $00$) puis vérifier que les _size_ cases suivantes ont aussi un _tag_ nul. Si c'est le cas on écrit, sinon on reprend la recherche à partir de là. S'il n'y a plus de place, on renverra un pointeur nul (_ie_ le pointeur $0000$).

Pour effectuer un _free_, on a juste à écrire $00$ sur le premier octet de chaque case de la section mémoire réservée par notre objet ($size + 1$ cases). Ainsi chaque case de la section mémoire sera alors considérée comme une case libre (_tag_ $00$).

= Le compilateur
== La chaîne de compilation

L'objectif de ce compilateur est de recevoir en entrée le code _skel_ d'une machine abstraite et de renvoyer le code Uxn compilé de cette machine abstraite.

Pour ce faire, on va définir plusieurs représentations sur lesquelles le compilateur travaillera. La première représentation est celle obtenue à partir du code _skel_ via un AST (_Abstract Syntactic Tree_). La seconde est une représentation intermédiaire, que l'on nommera IR (_Intermediate Representation_), qui abstrait et simplifie le code reçu. Et enfin, la dernière représentation est la plus proche du langage d'Uxn.

Entre chaque représentation, on appliquera des transformations sur la représentation courante avant de la traduire et la transmettre à la représentation suivante.

Au début, on reçoit le code _skel_ traduit pour notre AST. À la fin, une fois arrivé à la représentation en Uxn, on peut la compiler vers le langage assembleur d'Uxn.

On obtient alors la chaîne de compilation représentée en @compile_chain.

#place(
  bottom + center,
  scope: "parent",
  float: true,
)[
  #figure(caption: [Chaîne de compilation])[
    #box(stroke: (top: .5pt + silver) ,raw-render(
      ```
      digraph {
        rankdir=LR
        node[shape=box]
        v[shape=point, width=0, label="", style=invis]
        v -> AST
        AST -> AST
        AST -> IR
        IR -> Uxn
        Uxn -> "assembleur"
      }
      ```,
      edges : (
        "AST": ("AST" : "Normalize", "IR": "IrFromAstNormalized"),
        "IR" : ("Uxn" : "UxnFromIr"),
        "Uxn" : ("assembleur": "Compile")
      ),
      width: 98%,
    ))
   ] <compile_chain>
]

== Première couche : l'AST

#let languageColor3 = yellow.darken(50%)
#let type = $t y p e$
#let Var = $coloring("Var", #languageColor3)$
#let Tuple = $coloring("Tuple", #languageColor3)$
#let Object = $coloring("Object", #languageColor3)$
#let FunCall = $coloring("FunCall", #languageColor3)$
#let matchExpr = $m a t c h E x p r$
#let MatchVar = $coloring("MatchVar", #languageColor3)$
#let MatchTuple = $coloring("MatchTuple", #languageColor3)$
#let MatchObject = $coloring("MatchObject", #languageColor3)$
#let languageColor4 = fuchsia.darken(30%)
#let statement = $s t a t e m e n t$
#let Let = $coloring("let", #languageColor4)$
#let In = $coloring("in", #languageColor4)$
#let Match = $coloring("match", #languageColor4)$
#let With = $coloring("with", #languageColor4)$
#let MatchArrow = $coloring(->, #languageColor4)$

Pour créer l'AST, j'ai construit la grammaire que l'on pourrait décider d'utiliser pour _parser_ les codes _skel_ des machines abstraites. Une machine abstraite peut se représenter comme un tableau de transitions enrichi par des fonctions, internes et externes, et muni d'une fonction de transition principale.

Ainsi, une machine abstraite $cal(M)^sharp$ est représenté par un quintuplet $chevron e_T, e_F, T, F, delta chevron.r$ tel que :
- $e_T$ est l'ensemble des types externes,
- $e_F$ est l'ensemble des fonctions externes, 
- $T$ est l'ensemble des types internes,
- $F$ est l'ensemble des fonctions internes,
- $delta$ est la transition principale de $cal(M)^sharp$.

Les externes, types et fonctions, n'ont pas de définition explicite dans le code de $cal(M)^sharp$. Ces externes peuvent provenir d'Uxn (type : octet ou _short_, fonction : les _opcodes_) ou provenir d'une autre machine abstraite dont dépend alors $cal(M)^sharp$. Les externes sont alors juste représentés par une liste de noms associés à aucune définition.

Un type interne $t in T$ est représenté par son nom et la liste de ses constructeurs.

Une fonction interne $f in F$ est représentée par son nom, ses arguments, son typage et son corps. Le corps d'une fonction est un _statement_, défini en @statement_gram.

La transition principale $delta$ est définie comme une fonction à ceci-près que son corp est directement _statement_ $Match x With [...]$ (@statement_gram).

La grammaires définissant les _statement_ (@statement_gram), dép des grammaires définissant les _expressions_ (@expr_gram) et les _matching expressions_ (@matchExpr_gram).

Pour les définitions suivantes, $x, o$ et $f$ sont des chaînes de caractères, on notera $[ type ]$ une liste faite de mots de $cal(L)(type)$ et on notera $MatchArrow(type_1, type_2)$ un element de $cal(L)(type_1) times cal(L)(type_2)$.


#figure(caption: [
  Grammaire des _expressions_ de l'AST
])[
  $
    expr ::=& Var(x)\
    |& Tuple([expr])\
    |& Object(o, [expr])\
    |& FunCall(f, [expr])
  $
] <expr_gram>

#figure(caption: [
  Grammaire des _matching expressions_ de l'AST
])[
  $
    matchExpr ::=& MatchVar (x)\
    |& MatchTuple([matchExpr])\
    |& MatchObject(o, [matchExpr])
  $
] <matchExpr_gram>

#figure(caption: [
  Grammaire des _statements_ de l'AST
])[
  $
    #statement ::=
    & #expr\
    |& Let x = expr In statement\
    |& Match x With\ &quad [matchExpr MatchArrow statement]
  $
] <statement_gram>

== Normalisation de l'AST
Dans l'état actuel, il n'est pas pratique de traduire directement le code vers Uxn. En effet, le problème vient des _matching expressions_. Comme ces expressions sont récursives, on peut être amené à faire un match récursif, ce qui n'est pas pratique à coder en Uxn. Le problème est qu'on devrait progressivement charger le premier _short_ de l'objet afin de récupérer son _header_, ce qu'on utilisera pour le matching.

L'objectif est alors de transformer les _statements_ pour que les _matching expressions_ n'ai plus qu'une hauteur au plus égale à un. Où la hauteur est vu classiquement sur les arbres, avec les $#MatchVar (x)$ qui sont les feuilles de hauteur 0.

#figure(kind: "ex", supplement: "ex.", caption: [normalisation de match (en OCaml).])[
  #algo(comment-prefix: "")[
    $Match a With$\
    #coloring("|", languageColor4) #coloring("Cons1", languageColor3) (#coloring("Cons2", languageColor3) $b$, $c$) #MatchArrow $s_1$\
    #coloring("|", languageColor4) #coloring("Cons1", languageColor3) (#coloring("Cons3", languageColor3) ($d$, $e$), $f$) #MatchArrow $s_2$\
    #coloring("|", languageColor4) ...
  ]
  $ #rotate(90deg,$arrow.squiggly$) "Normalize" #rotate(90deg,$arrow.squiggly$)$ 
  #algo(comment-prefix: "")[
    $Match a With$\
    #coloring("|", languageColor4) #coloring("Cons1", languageColor3) ($x$, $y$) #MatchArrow ( #i\
      $Match x With$\
    #coloring("|", languageColor4) #coloring("Cons2", languageColor3) $b$ #MatchArrow $s_1{c := y}$\
    #coloring("|", languageColor4) #coloring("Cons3", languageColor3) ($d$,$e$) #MatchArrow $s_2{f := y}$\
    ...#d\
    )\
    #coloring("|", languageColor4) ...
  ]
] <ex_normalisation_match>

En prenant l'exemple précédent (@ex_normalisation_match), le _statement_ initial est normalisé en le _statement_ final. On remarque alors qu'on a besoin d'introduire des variables fraîches et de fusionner des variables sous un même nom, grâce aussi à des variables fraîches. Il faut alors substituer dans le _statement_ résultant du cas du match l'ancienne variable par la nouvelle.

Ainsi, cette normalisation va être découpée en deux phases : une première de décurrification puis une seconde de fusion.

#figure(table(
  columns: 2,
  fill: (x,y) => if (y==0) {silver},
  $m_e$, $"decurrify"(m_e,s)$,
  $MatchVar(x)$, $(m_e, s)$,
  $MatchTuple(l)$, [
    $(MatchTuple(l'), s')$\
    avec :\
    $(l', s') = "fold_right"("rebuild", l, ([], s))$
  ],
  $MatchObject(o, l)$, [
    $(MatchObject(o, l'), s')$\
    avec :\
    $(l', s') = "fold_right"("rebuild",l,([], s))$
  ]
), caption: [
  définition de la fonction _decurrify_
])

#figure(table(
  columns: 2,
  rows: (auto, auto, auto, auto),
  fill: (x,y) => if (y==0) {silver},
  $m_e$, $"rebuild"(m_e, (l,s))$,
  $MatchVar(x)$, $(m_e :: l, s)$,
  $MatchTuple(l')$,
  table.cell(rowspan: 2)[
    $(" "&MatchVar(x) :: l,\ &Match x With [m'_e MatchArrow s']" ")$\
    avec :\
    $(m'_e, s') = "decurrify"(m_e, s)$\
    $x$ une variable fraîche
  ],
  $MatchObject(o, l')$
),caption: [
  défintion de la fonction _rebuild_
])

Pour effectuer la première phase de décurrification, on fait appel à deux fonctions mutuellement récursives : _decurrify_ et _rebuild_. La fonction _decurrify_ remplace les arguments des _matching expressions_ par des arguments qui ne sont que sous la forme $MatchVar(x)$ obtenus à partir de la fonction _rebuild_. Cette deuxième fonction prend en entrée une _matching expression_, la liste des arguments déjà reconstruits et le corps du _match_ puis renvoie la nouvelle liste d'arguments, agrandie de un élément, et le corps du _match_ potentiellement étendu par un autre _match_ qui aura été passé dans _decurrify_.

À présent, il va falloir fusionner les cas du _match_. En effet, notre transformation introduit des cas de _matching_ sur un même constructeur qui n'opère alors plus que sur des variables libres. Ainsi, seul le premier cas de chaque constructeur peut être visité. L'algorithme de fusion est assez intuitif, on renomme les arguments de constructeurs pour que tous les arguments en même position aient le même nom puis on substitue dans le corps du _match_. Enfin, on fusionne les branches et on itère récursivement.

On obtient alors une chaîne de normalisation, comme représentée en @ex_normalisation_process, qui nous permet d'avoir un programme dont les _matching expressions_ ont bien une hauteur au plus égale à un.


#figure(kind: "ex", supplement: "ex.", caption: [chaîne de normalisation de match (en OCaml).])[
  #algo(comment-prefix: "")[
    $Match a With$\
    #coloring("|", languageColor4) #coloring("Cons1", languageColor3) (#coloring("Cons2", languageColor3) $b$, $c$) #MatchArrow $s_1$\
    #coloring("|", languageColor4) #coloring("Cons1", languageColor3) (#coloring("Cons3", languageColor3) ($d$, $e$), $f$) #MatchArrow $s_2$\
    #coloring("|", languageColor4) ...
  ]
  $ #rotate(90deg,$arrow.squiggly$) "decurrify" #rotate(90deg,$arrow.squiggly$)$ 
  #algo(comment-prefix: "")[
    $Match a With$\
    #coloring("|", languageColor4) #coloring("Cons1", languageColor3) ($x_1$, $c$) #MatchArrow ( #i\
      $Match x_1 With$\
    #coloring("|", languageColor4) #coloring("Cons2", languageColor3) $b$ #MatchArrow $s_1$#d\
    )\
    #coloring("|", languageColor4) #coloring("Cons1", languageColor3) ($x_2$, $f$) #MatchArrow ( #i\
      $Match x_2 With$\
    #coloring("|", languageColor4) #coloring("Cons3", languageColor3) ($d$,$e$) #MatchArrow $s_2$#d\
    )\
    #coloring("|", languageColor4) ...
  ]
  $ #rotate(90deg,$arrow.squiggly$) "merge" #rotate(90deg,$arrow.squiggly$)$ 
  #algo(comment-prefix: "")[
    $Match a With$\
    #coloring("|", languageColor4) #coloring("Cons1", languageColor3) ($x_1$, $y$) #MatchArrow ( #i\
      $Match x_1 With$\
    #coloring("|", languageColor4) #coloring("Cons2", languageColor3) $b$ #MatchArrow $s_1$\
    #coloring("|", languageColor4) #coloring("Cons3", languageColor3) ($d$,$e$) #MatchArrow $s_2{x_2 := x_1, f := c}$\
    ...#d\
    )\
    #coloring("|", languageColor4) ...
  ]
] <ex_normalisation_process>

== Seconde couche : l'IR

Cette représentation intermédiaire sert à simplifier la représentation précédente, tout en gardant la même expressivité, afin de simplifier la traduction vers la couche suivante, la représentation d'un programme Uxn.

Ainsi, la traduction de l'AST vers l'IR est assez immédiate. Il y a quelques renommage en cours de route, comme les arguments d'un objets dans les _matching expressions_ qui sont alors renommée en le nom de la variable auquel on ajoute en suffixe le numéro sa position dans les arguments.

L'intérêt principale de cette représentation est la traduction qui est faite pour les types. À présent, un type n'est plus que définit par la liste de ses constructeurs associé au nombre d'arguments qu'il requiert.

Dans le même temps, les tuples deviennent des objets normaux. Le constructeur est alors "Tuple" et est associé au nombre d'éléments que contient le tuple.

== Dernière couche : Uxn

Dans cette représentation, on intègre tout ce qui sera nécessaire à la compilation vers Uxn.

Ainsi, on intègre à présent la pile dans la définition, afin de simuler l'état de cette dernière au fil de l'exécution. De plus, comme on n'a plus exactement des variables, on créer un type $v a r$ qui est soit une variable qui est alors stockée dans la pile, soit un accès mémoire, où l'on empile alors le résultat.

Comme petite modification, maintenant on distingue les appels de fonction des _opcodes_ d'Uxn.

La plus grosse transformation s'opère encore sur les _matchs_. À présent, chaque corps de cas d'un _match_ est un appel à une fonction qui elle contient le code de l'action a effectuer.

#figure(kind: "ex", supplement: "ex.", caption: [transformation de match (en OCaml), avec $s'$ étant le résultat de la transformation de $s$.])[
  #algo(comment-prefix: "")[
    $Match a With$\
    #coloring("|", languageColor4) #coloring("Cons", languageColor3) (...) #MatchArrow $s$\
    #coloring("|", languageColor4) ...
  ]
  #rotate(90deg, $arrow.squiggly$)
  #grid(
    columns: 2,
    align: center+horizon,
    column-gutter: 5pt,
    algo(comment-prefix: "", strong-keywords: false)[
      $Let f () = s'$
    ],
    algo(comment-prefix: "", strong-keywords: false)[
      $Match a With$\
      #coloring("|", languageColor4) #coloring("Cons", languageColor3) #MatchArrow $f ()$\
      #coloring("|", languageColor4) ...
    ]
  )
]

Aussi, tous les noeuds de notre arbre syntaxique sont annoté par l'état de la pile avant leur exécution et l'état théorique après leur exécution.

== Export final

Il nous faut d'abord exporter les types. Ils seront représenté comme un macro en Uxn. Une macro en Uxn est un mot qui est associé à un suite d'instructions et qui remplace statiquement ses usages à l'assemblage. Avec le travail que l'on a effectué jusqu'à présent, nos types et nos constructeurs correspondent presque aux _headers_ des objets représenté en mémoire, _cf_ @repr_obj. Il faut juste récupérer l'index du constructeur dans la liste de son type ainsi que le nombre d'argument et l'on a notre macro.

#figure(kind: "ex", supplement: "ex.", caption: [Compilation d'un type])[
  $"configuration" = "Constructors"&[ "Eval"(2)\
  &; "Apply"(2)\
  &; "Stop"(1)\
  &; "Main"(1)]$
  #rotate(90deg, $arrow.squiggly$)
  #algo(comment-prefix: "", strong-keywords: false)[
    #comment(inline: true)[( configuration )]\
    %Eval { \#0102 }\
    %Apply { \#0202 }\
    %Stop { \#0301 }\
    %Main { \#0401 }\
  ]
] <ex_type_compilation>

Comme on le voit en @ex_type_compilation, le constructeur Eval est le premier de la liste, donc il commence par 01, puis il prend 2 arguments, d'où 0102.

Ensuite, il faut compiler les expressions (valeur, création d'objets et appel de fonctions). Cette phase là se fait assez facilement avec les _opcodes_ d'Uxn tout en manipulant les piles. Deux cas restent néanmoins non-triviaux, le cas des valeurs (variables ou accès mémoire).


Une valeur est soit une variable qui est déjà présente dans la pile de travail soit un accès mémoire. Pour récupérer un variable dans la pile de travail, on dépile la pile de travail dans la pile de retour (opération permise en Uxn) jusqu'à arriver à notre variable, qu'on duplique (opération permise en Uxn) puis on fait l'inverse, on dépile la pile de retour dans la pile de travail sous notre variable, et l'on s'arrête une fois que la pile de retour a retrouvé son état initial. Si c'est un accès mémoire, le #box[$i$#super("ème")] élément d'un objet, on récupère le pointeur dans la pile auquel on ajoute $2 times (i+1)$. On ajoute un car #box($i = 0$) correspond au _header_ de notre objet et on multiplie par 2 car on peut accéder à chaque octet de la mémoire, or l'on a décidé de ne considérer que des _shorts_.

Ensuite, il faut compiler les _statements_. Là aussi c'est assez naturel, seul le cas des _matchs_ est intéressant à détailler.

Dans notre représentation actuelle, un _match_ n'est plus qu'une valeur qui admet une liste de cas représentés seulement par le nom du constructeur et où pour chaque cas on saute vers la bonne fonction. Ainsi, on va comparer le constructeurs avec le _header_ de notre objet, si c'est les mêmes on saute vers la fonction, sinon, on passe au cas suivant.

#figure(kind: "ex", supplement: "ex.", caption: [Compilation d'un match sur un type])[
  #algo(comment-prefix: "", strong-keywords: false)[
    \@step#i\
      LDA2k Apply EQU2 ;step\<Apply> JCN2\
      LDA2k Eval EQU2 ;step\<Eval> JCN2\
      LDA2k Main EQU2 ;step\<Main> JCN2\
      LDA2k Stop EQU2 ;step\<Stop> JCN2
  ]
] <ex_match_compilation>

Détaillons l'exemple précédent (@ex_match_compilation), qui est un match sur le type présenté avant (@ex_type_compilation). On est dans la fonction _step_, symbolisé par le "@" devant le nom, et l'on fait un match sur l'objet que l'on reçoit. Comme l'objet que l'on reçoit est un pointeur vers la mémoire comme vu en @repr_obj, sa première case mémoire est le _header_. En faisant LDA2k, on empile le _header_ en conservant l'objet en dessous. Ensuite on empile le _header_ des objets construits par _Apply_ puis on les compare, on consomme les 2 _headers_ et il ne reste plus qu'un octet représentant un booléen. Finalement, on empile l'adresse de la fonction qui gère le cas _Apply_ et on saut à cette adresse si le booléen est vrai, le booléen et l'adresse sont consommés. Sinon, on passe à la ligne suivante et ça marche pareil.


= Ce qu'il reste à faire

Je dois rajouter le _free_ et je dois modifier le _malloc_ pour qu'il recherche vraiment de l'espace libre car pour le moment c'est une simple pile avec un _push_.

Je dois aussi corriger les quelques bug qui restent.

