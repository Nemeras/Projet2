open List

let clause_active c v empl i =
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

let clause_inactive c v empl i =
	let rec aux c =
		match c with
		| [] -> ()
		| x::q when x > 0 ->
			empl.(x) <- (filter (fun y -> y <> i) (fst empl.(x))), snd empl.(x) ;
			aux q ;
		| x::q ->
			empl.(-x) <- fst empl.(-x), (filter (fun y -> y <> i) (snd empl.(-x))) ;
			aux q ;
	in
	aux c

type stack = (int*(int list)) list ref

let create_stack() = ref [(0,[])];;

let is_empty liste = match !liste with
|[]->true
|_->false;;

let pick stack =
	fst (List.hd !stack)

let backtrack_changer n achanger tableau empl =
let continue = ref true in
while !continue do
	match !achanger with
	|[]->continue:=false;
	|h::t->let (_,liste) =tableau.(h) in tableau.(h) <- (false,liste);clause_active liste tableau empl h;achanger:=t;
done;;

let rec backtrack_rajouter n arajouter tableau =
match arajouter with
|[]-> ();
|h::t -> let (boole,liste)=tableau.(h) in tableau.(h) <- (boole,n::liste);
		backtrack_rajouter n t tableau ;;

let backtrack pil tableau empl arajouter=
let pile = !pil in
match pile with
|[]-> failwith "Pile vide";
|(n,achanger)::t -> backtrack_changer n (ref achanger) tableau empl;pil:=t;backtrack_rajouter (-n) arajouter tableau;n;;

let rec update_supprimer n pil tableau liste=
match liste with
|[] -> ();
|h::t -> let (boole,liste)=tableau.(h) in tableau.(h) <- (boole,(List.filter (fun i -> i!=n) liste));
		if tableau.(h)=(boole,[]) then begin tableau.(0)<- false,[]; update_supprimer n pil tableau t end else update_supprimer n pil tableau t;;

let rec update_changer n pil tableau empl liste=
match liste with
|[]-> [];
|h::t -> let (boole,liste)=tableau.(h) in if not boole then (tableau.(h) <- (true,liste);clause_inactive liste tableau empl h);(h::(update_changer n pil tableau empl t));;


let update n pil tableau empl liste listeoppose=
let liste_changement = update_changer n pil tableau empl listeoppose in
update_supprimer (-1*n) pil tableau liste;
pil:=(n,liste_changement)::(!pil);;
