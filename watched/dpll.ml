open Cnf
open List
open Array
open Pile
open Watched


let rec delete_uni clauses solution =
	match clauses with
	| [] -> []
	| [x]::tail when solution.(abs x)*x >=0 ->
		solution.(abs x) <- 2*x ;
		delete_uni tail solution
	| [x]::tail ->
		solution.(0) <- -2 ;
		[]
	| []::tail ->
		solution.(0) <- -2 ;
		[]
	| c::tail -> c::(delete_uni tail solution)

let cnf_to_vect clauses nbr_clauses =
	let v = make nbr_clauses [] in
	let rec aux l i =
		match l with
		| [] -> ()
		| c::q ->
			v.(i) <- c ;
			aux q (i+1) ;
	in
	aux clauses 0 ;
	v;;


let rec propa uni stack v solution =
	match uni with
	| [] -> ()
	| x::q when x > 0 ->
		solution.(x) <- 2 ;
		let l = update x stack v solution in
		propa (l@q) stack v solution
	| x::q ->
		solution.(-x) <- -2 ;
		let l = update x stack v solution in
		propa (l@q) stack v solution

let continue stack v solution uni k back =
	if solution.(0) < 0 && not !back then
		begin
		uni := [] ;
		back := true
		end
	else if !back then
		if !k > 0 && solution.(!k) = 1 then
			begin
			solution.(0) <- 0 ;
			back := false ;
			backtrack stack ;
			k := - !k ;
			solution.(- !k) <- -1 ;
			uni := update !k stack v solution ;
			end
		else if !k > 0 then
			begin
			backtrack stack ;
			k := pick stack
			end
		else
			begin
			backtrack stack ;
			k := pick stack
			end
	else
		begin
		propa !uni stack v solution ;
		uni := [] ;
		k := abs !k + 1 ;
		if abs solution.(!k) <= 1 then
			begin
			solution.(!k) <- 1 ;
			uni := update !k stack v solution ;
			end
		end


let solve cnf =
	ordo cnf ;
	let solution = make (cnf.v_real+1) 0 in
	let clauses = delete_uni cnf.clauses solution in
	if solution.(0) < 0 then
		False
	else
		begin
		let v = cnf_to_vect clauses (List.length clauses) in
		let stack = create_stack () in
		let k = ref 1 in
		while solution.(!k) < 0 do
			incr k
		done ;
		if !k <= cnf.v_real then
			begin
			solution.(!k) <- 1 ;
			let truc = update !k stack v solution in ()
			end
		;
		let back = ref false in
		let uni = ref [] in
		while abs !k <= cnf.v_real && !k <> 0 do
			if abs !k = cnf.v_real then
				if solution.(0) < 0 then
					continue stack v solution uni k back
				else
					k := cnf.v_real + 1
			else
				continue stack v solution uni k back
		done ;
		if !k = 0 then
			False
		else
			True solution
		end
