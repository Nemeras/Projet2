			(** LECTURE DU FICHIER ET LANCEMENT DE L'ALGORITHME **)



open Cnf



(* Lit le fichier *)
let lexbuf file =
	Lexing.from_channel (open_in file)


(* Interprète le fichier *)
let parse file = Parser.cnf Lexer.token (lexbuf file)


(* Crée la CNF représentée dans le fichier *)
let create file =
	try
		parse file
	with _ -> (failwith "Erreur de saisie")


(* Fonction principale *)
let _ =
	(*(* Gestion des arguments et des options *)
	let file = ref "" in	(* Nom du fichier d'entrée *)
	let opt = ref false in	(* True ssi on lance la version optimisée *)
	let options = [("-opt", Arg.Set opt, "Exécute la version optimisée")] in
	Arg.parse options (fun s -> file := s)  "Ce programme résout l'instance de SAT donnée dans le fichier en entrée." ;*)
	
	(* Récupère la CNF à analyser *)
	let cnf = create !file in
	
	(* Affiche les warnings sur le nombre de clauses et de varaibles *)
	if cnf.v_real <> cnf.v then
		Printf.printf "Attention : L'indice maximal des variables est %d, alors que le nombre annoncé était %d\n" cnf.v_real cnf.v ;
	if cnf.c_real <> cnf.c then
		Printf.printf "Attention : Le fichier comporte %d clauses, alors que %d clauses étaient annoncées\n" cnf.c_real cnf.c ;
	
	(* Lance le programme principal *)
	(*if !opt then
		print_solution (DavisPutnam_opt.solve cnf)
	else*)
		print_solution (Dpll.solve cnf)
