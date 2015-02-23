open List

let is_false n solution = solution.(abs n)*n < 0;;
let is_true n solution = solution.(abs n)*n > 0;;

let rec aufond liste solution=
match liste with
|h::h2::tail when is_false h2 solution -> liste
|h::h2::tail -> h2::(aufond (h::tail))
|[h]->liste
|[]->[];;

let is_w_true liste solution=
match liste with
|h::h2::_ when is_true h solution -> true
|h::h2::_ when is_true h2 solution -> true
|h::h2::_ -> false;;
|_-> failwith "probleme dans is_w_true";;

let cannot_find_w liste solution=
match liste with
|h::h2::_ when is_false h2 solution -> true
|h::h2::_ -> false
|_ -> failwith "probleme dans cannot find";;

let change_clause liste solution =
match liste with
|h::h2::_ when (is_false h solution) && (is_false h2 solution) -> aufond (h::(aufond (tl liste) solution)) solution
|h::h2::_ when is_false h solution -> aufond liste solution
|h::h2::_ when is_false h2 solution -> h::(aufond (tl liste) solution)
|h::h2::_ -> liste
|_->failwith "probleme dans change_clause";;


