open Cnf
open List
open Array
open Pile


let init_clause c v empl i =
	let rec aux c =
		match c with
		| [] -> ()
		| x::q when x > 0 ->
			empl.(x) <- i::(fst empl.(x)), snd empl.(x) ;
			aux q ;
		| x::q ->
			empl.(-x) <- fst empl.(-x), i::(fst empl.(-x)) ;
			aux q ;
	in
	aux c

let cnf_to_vect cnf =
	let v = make cnf.c_real (false,[]) in
	let empl = make (cnf.v_real+1) ([],[]) in
	let rec aux l i =
		match l with
		| [] -> ()
		| c::q ->
			init_clause c v empl i ;
			v.(i) <- false, c ;
			aux q (i+1) ;
	in
	aux cnf.clauses 1 ;
	v.(0) <- true, [] ;
	v, empl
		
		

let solve cnf =
	Cnf.ordo cnf ;
	let v, empl = cnf_to_vect cnf in
	let solution = make cnf.v_real (-1) in
	let stack = create_stack() in
	(* ajouter (1,_,_) à la pile *)
	let k = ref 1 in
	while not ((is_empty) stack) do
()
		(* Si k < 0 -> on dépile *)
		(* Si tout est à true -> on interrompt *)
		(* Si le 1er élt est à faux -> On dépile *)
	done ;
	(* Si un littéral est présent seulement positivement/négativement : on le met à vrai/faux
	   Sinon :
		On fait l'étape suivante
		Si liste vide : on dépile
		Sinon : satisfiable et on garde l'instanciation
	*)
	False
