open Cnf
open List
open Array
open Pile


let print_tab v =
	for i = 0 to length v - 1 do
		if not (fst v.(i)) then
			print_string (string_of_clause (snd v.(i)))
		else
			begin
			print_string "INACTIVE     " ;
			print_string (string_of_clause (snd v.(i)))
			end
	done

let print_empl empl =
	for i = 0 to length empl - 1 do
		print_int i ; print_newline () ;
		print_string (string_of_clause (fst empl.(i))) ;
		print_newline() ;
		print_string (string_of_clause (snd empl.(i))) ;
		print_newline() ;
	done

let init_clause c v empl i =
	let rec aux c =
		match c with
		| [] -> ()
		| x::q when x > 0 ->
			empl.(x) <- i::(fst empl.(x)), snd empl.(x) ;
			aux q ;
		| x::q ->
			empl.(-x) <- fst empl.(-x), i::(snd empl.(-x)) ;
			aux q ;
	in
	aux c

let cnf_to_vect cnf =
	let v = make (cnf.c_real+1) (false,[]) in
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


(*let continue stack v empl solution k =
	print_int !k ; print_newline() ;
	if not (fst v.(0)) && !k < 0 then
		begin
		v.(0) <- true, [] ;
		solution.(- !k) <- 0 ;
		k := backtrack stack v (snd empl.(- !k)) ;
		end
	else if not (fst v.(0)) && !k > 0 then
		begin
		v.(0) <- true, [] ;
		k := backtrack stack v (fst empl.(!k)) ;
		update (- !k) stack v (fst empl.(!k)) (snd empl.(!k)) ;
		k := - !k ;
		solution.(- !k) <- -1 ;
		end
	else if !k <> 0 then
		begin
		k := abs !k + 1 ;
		update !k stack v (snd empl.(!k)) (fst empl.(!k)) ;
		solution.(!k) <- 1 ;
		end
*)

let continue stack v empl solution k back =
	if not (fst v.(0)) && not !back then
		back := true
	else if !back then
		if !k > 0 then
			begin
			v.(0) <- true, [] ;
			back := false ;
			backtrack stack v (snd empl.(!k)) ;
			k := - !k ;
			update !k stack v (fst empl.(- !k)) (snd empl.(- !k)) ;
			solution.(- !k) <- -1 ;
			end
		else
			begin
			backtrack stack v (fst empl.(- !k)) ;
			k := pick stack
			end
	else
		begin
		k := abs !k + 1 ;
		update !k stack v (snd empl.(!k)) (fst empl.(!k)) ;
		solution.(!k) <- 1 ;
		end


let solve cnf =
	ordo cnf ;
	let v, empl = cnf_to_vect cnf in
	let solution = make (cnf.v_real+1) 0 in
	let stack = create_stack() in
	update 1 stack v (snd empl.(1)) (fst empl.(1)) ;
	solution.(1) <- 1 ;
	let k = ref 1 in
	let back = ref false in
	while abs !k <= cnf.v_real && !k <> 0 do
		if !k <> 0 then
			if abs !k = cnf.v_real then
				if not (fst v.(0)) then
					continue stack v empl solution k back
				else
					k := cnf.v_real + 1
			else
				continue stack v empl solution k back
	done ;
	(* Si un littéral est présent seulement positivement/négativement : on le met à vrai/faux
	   Sinon :
		On fait l'étape suivante
		Si liste vide : on dépile
		Sinon : satisfiable et on garde l'instanciation
	*)
	if !k = 0 then
		False
	else
		True solution
