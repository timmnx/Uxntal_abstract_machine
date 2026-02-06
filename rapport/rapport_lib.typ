#import "lib.typ": ieee
#import "@preview/diagraph:0.3.4": raw-render
#import "@preview/algo:0.3.6": algo, i, d, comment
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

// #show figure: it => align(center, it)

#show: ieee.with(
  title: [Compilation de machine abstraite vers UXN],
  abstract: [ #lorem(200)
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
  index-terms: ("Machines Abstraites", "Compilation","UXN", "AST"),
  bibliography: bibliography("refs.bib"),
)
// #set text(size: 11pt)
// Your content goes below.
= Introduction
_*Contexte.*_
Les machines abstraites prennent la forme d'interpréteurs récursifs terminaux et de premier ordre, sous forme d'un système de transitions @fun_cor_am. Elles constituent le format de sémantique le plus précis et le plus bas niveau ; et sont donc particulièrement adaptées pour une implémentation directe en assembleur. UXN est une architecture de jeu d’instructions minimale (elle ne contient que peu d’instructions) et open source.

_*Objet de recherche.*_
Le projet consiste à développer un compilateur prenant en entrée une description de machine abstraite et produisant en sortie une implémentation de cette machine en assembleur UXN.

_*Contributions.*_
Le projet se déroulera en deux phases. Une première phase sera l’implémentation à la main d’une machine simple en UXN. La deuxième phase consistera en la généralisation et l’automatisation de cette transformation, en utilisant nos outils de manipulations de sémantiques @skeletons. Si le temps le permet, la transformation pourra être prouvée correcte (sur papier).

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
]

#figure(caption: [Syntaxe des continuations])[
  $
    cont ::=& Id\
    |& Aradd (expr, cont)\
    |& Fnadd (expr, cont)\
    |& Arsub (expr, cont)\
    |& Fnsub (expr, cont)
  $
]

On notera $cal(L)(expr)$ et $cal(L)(cont)$ comme étant les langages reconnus par les grammaires $expr$ et $cont$.

== Sémantique concrète
Nos expressions admettent une sémantique concrète, celle de l'arithmétique. On évalue ces termes avec la fonction $[|dot|]_(s c) : cal(L)(expr) --> ZZ$ définie comme suit :

#figure(caption: [Sémantique concrète])[
  $
    [|Int(i)|]_(s c) &= i\
    [|Add(e_1, e_2)|]_(s c) &= [|e_1|]_(s c) + [|e_2|]_(s c)\
    [|Sub(e_1, e_2)|]_(s c) &= [|e_1|]_(s c) - [|e_2|]_(s c)

  $
]

== Sémantique de la machine abstraite
#let eval = $e v a l$
#let Eval(e,k) = $angle.l #e | #k angle.r_bold(coloring(e,languageColor1))$
#let apply = $a p p l y$
#let Apply(e,k) = $angle.l #e | #k angle.r_bold(coloring(a,languageColor2))$
#let res = $r e s$
#let Res(i) = $angle.l #i angle.r_bold(coloring(r, #green.darken(20%)))$
#let fail = $coloring(F a i l, #red.darken(20%))$
#let step = $scripts(-->)^sharp$
#let steps = $scripts(-->)^(sharp*)$

Pour créer notre machine abstraite $cal(M)$, on la munie de trois états $eval$, $apply$ et $res$ ainsi que d'un système de transition $step$ étant une sémantique à petits pas.

#figure(caption: [États de la machine abstraite])[
  $
    eval &::= Eval(expr, cont) \
    apply &::= Apply(cont, expr) \
    res &::= Res(i) | fail & i in ZZ
  $
] <fig4>

#figure(caption: [Transitions pour les états $eval$])[
  $
    &Eval(Int(i), kappa)        &&step Apply(kappa, i)\
    &Eval(Add(e_1, e_2), kappa) &&step Eval(e_1, Aradd (e_2, kappa))\
    &Eval(Sub(e_1, e_2), kappa) &&step Eval(e_1, Arsub (e_2, kappa))
  $
] <fig5>

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
] <fig6>

En @fig6, les opérations "$+$" et "$-$" représentent le vrai calcul opéré par la machine. De plus, les états $res$ n'ont pas de successeur puisqu'ils symbolisent les états finaux de la machine.

On définit alors la clôture réflexive transitive de la relation de transition $step$ par $steps$. Pour deux états $sigma$ et $sigma'$, on a $sigma steps sigma'$ si et seulement si il existe une suite finie d'états $(sigma_i)_(0<=i<=n)$ telle que $sigma_0 = sigma$, $sigma_n = sigma'$ et pour tout $0<=i<n$, $sigma_i step sigma_(i+1)$.

On pose donc la fonction d'évaluation associée à la machine abstraite $[|dot|]^sharp : cal(L)(expr) --> ZZ$ comme étant le résultat de cette dernière sur l'entrée. Explicitement, on a $[|e|]^sharp = i$ si et seulement si $Eval(e, Id) steps Res(i)$.

= Au niveau machine
== Présentation rapide de UXN
En _UXN_, on manipule une machine à pile simple ayant un nombre restreint d'_opcodes_ (32 et 3 modes possibles) qui permettent de manipuler des octets (8 bits) ou des _shorts_ (2 octets, 16 bits). On a alors un espace mémoire adressable de $2^16$ octets ($64$ko) et deux piles de 256 octets, une de travail et une de retour.

Ainsi, en _UXN_ on écrit un code assembleur en notation polonaise inversée.

#figure(kind: "ex", supplement: [Exemple], caption: [calcul de $1+2$ en _UXN_ (en commentaire : l'état de la pile de travail).])[
  #algo()[
    |0100 #comment[$triangle.small.l$]#i\
      \#01 #comment[01 $triangle.small.l$]\
      \#02 #comment[01 02 $triangle.small.l$]\
      ADD #comment[03 $triangle.small.l$]\
      \#04 #comment[03 04 $triangle.small.l$]\
      MUL #comment[12 $triangle.small.l$]
  ]
]

Remarquons que sur l'exemple précédent, on peut déplacer des lignes sans que cela n'affecte le calcul. Par exemple, on pourrait remonter la ligne 5 en ligne 2 et tout ce passerait pareil. On aurait :
$
  #raw(lang:"c","// 04 01 02")
  -->_"ADD" #raw(lang:"c","// 04 03")
  -->_"MUL" #raw(lang:"c","// 12")
$

== Représentation machine des objets
#let tag = $t a g$
#let size = $s i z e$
#let data = $d a t a$
Pour la représentation des objets, j'ai décidé de m'inspirer de la représentation à la _OCaml_. Anisi, un objet est représenté en mémoire comme un triplet $angle.curly tag | size | data angle.r.curly$ où :
- $tag$ : indique le numéro de constructeur utilisé pour créer l'objet, codé sur un octet,
- $size$ : indique le nombre de _shorts_ réservés après, codé sur un octet,
- $data$ : est l'espace utilisable en mémoire, un tableau de $size$ shorts.
On remarque alors qu'un objet occupe $2times(size+1)$ octets, soit $size+1$ _shorts_. Ainsi, le champs _data_ peut contenuir au plus $255$ shorts (car $size <= 255 = 2^8-1$).

Il est intentionnel que les champs $tag$ et $size$ soient codés sur un octet chacun, ils sont contigus en mémoire et l'on ne travaillera qu'avec des _shorts_, ce qui est le cas de la concaténation de ces deux champs.

On appelle alors une case mémoire l'espace mémoire nécessaire pour stocker un _short_ alligné.

== Gestion mémoire
Afin de gérer la mémoire (de $64$ko), on va équiper notre machine d'un équivalent de _malloc_ et de _free_.

Le principe est simple. L'espace addressable est divisé en trois, une section _text_, une section _code_ et une section _raw_. Les sections _text_ et _code_ contiennent le code assembleur chargé et les différentes variables d'environnement (qui ne peuvent pas avoir de valeur par défaut dans la section _text_).

Il nous reste alors toute la section _raw_ qui peut être utilisée pour stocker dynamiquement des objets.

Lors d'un _malloc_, on va chercher la première case ayant un _tag_ nul (_ie_ le _tag_ vaut $00$) puis vérifier que les _size_ cases suivantes ont aussi un _tag_ nul. Si c'est le cas on écrit, sinon on reprend la recherche à partir de là. S'il n'y a plus de place, on renverra un pointeur nul (_ie_ le pointeur $0000$).

Pour effectuer un _free_, on a juste à écrire $00$ sur le premier octet de chaque case de notre objet ($size + 1$ cases), ainsi chaque case sera alors considérée comme une case libre (_tag_ $00$).

= Ce qu'il r_este à faire
== Terminer malloc-free
Je dois rajouter le _free_, ça ne devrait pas être long car c'est une simple boucle.

Je dois modifier le _malloc_ pour qu'il recherche vraiment de l'espace libre car pour le moment c'est une simple pile avec un _push_. Ça ne devrait pas être beaucoup plus long.

== Gérer les input
Pour le moment, le calcul à effectuer est écrit dans le _main_ du code. Ce serait bien de rendre le programme un minimum interactif, qu'il puisse prendre en _input_ via le terminal une expression et l'évaluer.

== AST et génération de code
Créer la syntaxe générale d'une machine abstraite et donc de l'AST qui lui correspond afin de pouvoir générer du code automatiquement, pour toute machine abstraite prise en entrée.