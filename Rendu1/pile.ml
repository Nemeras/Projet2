type stack = (int*(int list)) list ref

let create_stack() = ref [(0,[])];;

let is_empty liste = match !liste with
|[]->true
|_->false;;

let backtrack_changer n achanger tableau =
let continue = ref true in
while !continue do
	match !achanger with
	|[]->continue:=false;
	|h::t->let (_,liste) =tableau.(h) in tableau.(h) <- (false,liste);achanger:=t;
done;;

let rec backtrack_rajouter n arajouter tableau =
match arajouter with
|[]-> ();
|h::t -> let (boole,liste)=tableau.(h) in tableau.(h) <- (boole,n::liste);backtrack_rajouter n t tableau;;

let backtrack pil tableau arajouter=
let pile = !pil in
match pile with
|[]-> failwith "La pile etait vide connard";
|(n,achanger)::t -> backtrack_changer n (ref achanger) tableau;pil:=t;backtrack_rajouter (-1*n) arajouter tableau;n;;

let rec update_supprimer n pil tableau liste=
match liste with
|[] -> ();
|h::t -> let (boole,liste)=tableau.(h) in tableau.(h) <- (boole,(List.filter (fun i -> i!=n) liste));if tableau.(h)=(boole,[]) then tableau.(0)<-false; update_supprimer n pil tableau t else update_supprimer n pil tableau t;;

let rec update_changer n pil tableau liste=
match liste with
|[]-> [];
|h::t -> let (boole,liste)=tableau.(h) in if boole != true then tableau.(h) <- (true,liste);(h::(update_changer n pil tableau t));;


let update n pil tableau liste listeoppose=
let liste_changement = update_changer n pil tableau listeoppose in
update_supprimer (-1*n) pil tableau liste;
pil:=(n,liste_changement)::(!pil);;
