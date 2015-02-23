open List

type stack = int list ref

let create_stack() = ref [0];;

let is_empty liste = match !liste with
	|[]->true
	|_->false;;

let pick stack = (List.hd !stack)

let update element pile tableau solution=
	let liste_changement = ref [] in
	pile:=element::!pile;
	for int i = 0 to ((Array.length tableau)-1) do
	if !(is_w_true tableau.(i) solution) then
		begin
		tableau.(i) <- change_clause (tableau.(i)) solution;
		if is_random_then_false (tableau.(i)) solution then liste_changement:=(hd (tableau.(i)))::(!liste_changement);
		is_clause_false tableau.(i) solution
	end
done
liste_changement;;


let backtrack pile =
	let element = hd pile in
		pile:=tl !pile;
		element;;
