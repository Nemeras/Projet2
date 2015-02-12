open Array
open Cnf
open Pile

let solve cnf =
	Cnf.ordo cnf ;
	(*let clauses = to_vect cnf.clauses ;*)
	let solution = make cnf.v_real 0 in
	(* Créer pile *)
	(* Mettre 0,[],[] dans pile *)

	(* Tant que la pile est non vide *)
	(* Si un littéral est présent seulement positivement/négativement : on le met à vrai/faux
	   Sinon :
		On fait l'étape suivante
		Si liste vide : on dépile
		Sinon : satisfiable et on garde l'instanciation
	*)False
	;;
