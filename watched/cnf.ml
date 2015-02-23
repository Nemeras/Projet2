			(** CLAUSES ET CNF **)



open Printf
open List


type literal = int				(* Si x_k est une variable, k est son littéral positif et -k son littéral négatif *)

type clause = literal list

type cnf = {
	mutable clauses : clause list ;	(* La liste des clauses formant la conjonction *)
	v : int ;				(* Indice théorique (indiqué dans le fichier) maximal des variables *)
	mutable v_real : int ;			(* Indice maximal réel des variables *)
	c : int ;				(* Nombre théorique de clauses *)
	mutable c_real : int			(* Nombre réel de clauses *)
}

(* Représente la réponse de l'algorithme *)
type solution =
	| False			(* Insatisfiable *)
	| True of int array	(* Satisfiable, avec un tableau représentant un instanciation correspondante
				   (case k : valeur donnée à la variable d'indice k dans l'instanciation     *)



		(* CLAUSES *)

(* On considère que toutes les clauses sont triées par indices de variables décroissants,
   et qu'aucun littéral n'y apparait en double.
   Exemples : [], [3,2,-1], [3,-3,1].                                                     *)

(* Convertit une clause en une chaine de caractère se termiant par un saut de ligne *)
let rec string_of_clause c =
	match c with
	| [] -> "0\n"
	| x::c2 -> (string_of_int x) ^ " " ^ string_of_clause c2


(* Indique si la clause c triée est une tautologie *)
let trivial c =
	let rec aux c y =
		match c with
		| [] -> false
		| x::_ when x = -y -> true   (* Si un littéral et sa négation apparaissent, c'est une tautologie *)
		| x::q -> aux q x
	in
	aux c 0



		(* CNF *)


(* Convertit une CNF en string *)
let string_of_cnf cnf =
	(* Convertit une liste de clauses en string *)
	let rec clauses_to_string clauses =
		match clauses with
		| [] -> ""
		| c::q -> (string_of_clause c) ^ (clauses_to_string q)
	in
	"p cnf " ^ string_of_int cnf.v_real ^ " " ^ string_of_int cnf.c_real ^ "\n" ^ clauses_to_string cnf.clauses


(* Supprime les tautologies dans une liste de clauses *)
let rec delete_tautologies clauses =
	match clauses with
	| [] -> []
	| c::q when trivial c -> delete_tautologies q
	| c::q -> c::delete_tautologies q

(* Transforme la CNF en une CNF équivalente pour qu'elle s'adapte à la structure de données *)
let ordo cnf =
	(* Fonction de comparaisons de deux littéraux, cf les clauses triées plus haut *)
	let compare_lit x y =
		if abs x <> abs y then
			compare (abs y) (abs x)
		else
			compare y x
	in
	(* On trie chacune des clauses en enlevant les littéraux en double, puis on supprime les tautologies *)
	cnf.clauses <- map (fun l -> Sort.sort_uniq compare_lit l) cnf.clauses ;
	cnf.clauses <- delete_tautologies cnf.clauses



		(* SOLUTIONS *)


(* Affiche sur la sortie standard la solution sol *)
let print_solution sol =
	match sol with
	| False -> printf "s UNSATISFIABLE\n"
	| True t ->
		printf "s SATISFIABLE\n" ;
		for k = 1 to Array.length t - 1 do
			if t.(k) > 0 then
				printf "%d " k
			else
				printf "-%d " k
		done ;
		printf "0\n"