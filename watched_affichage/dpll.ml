open Cnf
open List
open Array
open Pile
open Watched
open Printf

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
;;

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
;;

let rec afficher_clause clause solution =
match clause with
|h::t -> printf "%d (%d)  " h (solution.(abs h));afficher_clause t solution;
|[] -> printf "\n";;

let afficher_t tableau solution =
for i=0 to ((Array.length tableau)-1) do
	afficher_clause (tableau.(i)) solution;
	printf "\n";
done;;

let continue stack v solution uni k back =
	let _ =
	if solution.(0) < 0 && not !back then
		begin
		uni := [] ;
		back := true;
		end
	else if !back then
		begin
		printf "Backtrack... \n";
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
				let _ = backtrack stack in
				k := pick stack;
				end
			else
				begin
				let _ = backtrack stack in
				k := pick stack;
				end
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
			in
			afficher_t v solution;;


let solve cnf =
	ordo cnf ;
	let solution = make (cnf.v_real+1) 0 in
	let clauses = delete_uni cnf.clauses solution in
	if solution.(0) < 0 then
		begin
		printf "False ici\n";
		False
		end
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
