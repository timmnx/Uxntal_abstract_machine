# 13 mars
## Done
- projet OCaml migré sous dune pour compiler plus facilement
- AST fonctionnel + printing

## ToDo
- dé-curryfier les match et les condenser, *ie*
	```ocaml
	match a with
	| Cons1 (Cons2 b, c) -> expr1
	| Cons1 (Cons3 (d,e), f) -> expr2
	| ...
	```
	&#8595;
	```ocaml
	match a with
	| Cons1 (x, y) -> begin
		match x with
		| Cons2 b -> expr1{c:=y}
		| Cons3 (d,e) -> expr2{f:=y}
		end
	| ...
	```
- Générer le CFG (blocs de calculs)
- Une fois le CFG fait, réfléchir à la génération de code

# 20 mars
## Done
- fonction `desugar` qui "unfold" les match, *ie*
	```ocaml
	match a with
	| Cons1 (Cons2 b, c) -> expr1
	| Cons1 (Cons3 (d,e), f) -> expr2
	| ...
	```
	&#8595; + introduction de variables fraîches (préfixe `%` n'étant pas autorisé pour l'user)
	```ocaml
	match a with
	| Cons1 (%2, %1) -> begin
		match %2 with
		| Cons2 b -> expr1{c:=%1}
		end
	| Cons1 (%4, %3) -> begin
		match %4 with
		| Cons3 (d,e) -> expr2{f:=%3}
		end
	| ...
	```
- fonction `merge` qui fusionne les match sur les mêmes constructeurs, *ie*
	```ocaml
	match a with
	| Cons1 (%2, %1) -> begin
		match %2 with
		| Cons2 b -> expr1{c:=%1}
		end
	| Cons1 (%4, %3) -> begin
		match %4 with
		| Cons3 (d,e) -> expr2{f:=%3}
		end
	| ...
	```
	&#8595;
	```ocaml
	match a with
	| Cons1 (%2, %1) -> begin
		match %2 with
		| Cons2 b -> expr1{c:=%1}
		| Cons3 (d,e) -> expr2{f:=%3}{%3:=%1}
		end
	| ...
	```

## ToDo
- Checker s'il ne faut pas aussi propager `{%4 := %2}` dans le merge pour le `Cons3`. Je crois que je le fais déjà, mais à vérifier.
- Checker ce que fais l'algo sur des cas de merge bizares (comme des tuples de pas la même taille, des variables libres, etc.)
- Faire une transformation de l'ast actuel vers la version simplifiée (donnée par Martin).

# 26 mars
## Done

- La traduction de l'`AST` vers l'`IR` a été faite. L'`IR` est simple et est définie comme suit :
	- le type expression est définie par soit une variable, un objet (un tuple étant un objet) ou un appel à une fonction (il est supposé que tous les arguments sont bien donnés) :
		```ocaml
		type exp =
		| Var of string
		| Obj of string * exp list
		| Call of string * exp list
		```
	- le type pattern pour le case, soit c'est un objet avec des arguents, soit c'est le cas par défaut :
		```ocaml
		type pat =
		| Pattern of string * string list
		| Default
		```
	- le type body, qui définit la structure du code d'une fonction :
		```ocaml
		type body =
		| Let of string * exp * body
		| Ret of exp
		| Case of string * (pat * body) list
		```
	- le type déclaration, permettant de déclarer des fonctions :
		```ocaml
		type decl =
		| Fun of string * string list * body
		```
	- le type programme, étant une suite de déclaration de fonctions :
		```ocaml
		type program = decl list
		```
- Printing de l'`IR` faite, par `ppx` via `[@@deriving show]` et à la main

## ToDo
- Same as last week

# Entre le 3 et le 10 avril

## Done
- Traduction faite de l'`IR` vers code `UXN`, sauf quelques cas.

## ToDo
- Modifier l'`IR` pour ne plus nommer des variables dans le _case matching_ mais plutôt remplacer les variables par leur indice dans l'objet, afin de pouvoir voir et faire des accès comme dans un tableau 