type 'a tree = Nil | Node of 'a * 'a tree * 'a tree

let rec size node = match node with
		| Node (value, nodeleft, noderight) -> 1 + (size nodeleft) + (size noderight)
		| _ -> 0

let rec height node = match node with
		| Node (value, nodeleft, noderight) ->
			if (size nodeleft) > (size noderight) then
				(1 + (height nodeleft))
			else begin
				(1 + (height noderight))
			end
		| _ -> -1

let is_bst root =
	let rec loop ?(parent=Nil) ?(exptd=true) node = match node with
		| Node (nval, nodeleft, noderight) ->
			(match parent with
				| Node (pval, pleft, pright) ->
					if ((pval > nval) = exptd) then
						(loop ~parent:node nodeleft && loop ~parent:node ~exptd:false noderight)
					else false
				| Nil -> (loop ~parent:node nodeleft && loop ~parent:node ~exptd:false noderight))
		| Nil -> true
	in
	loop root

let is_perfect root =
	let rec loop ?(parent=Nil) ?(exptd=true) node = match node with
		| Node (nval, nodeleft, noderight) ->
			(match parent with
				| Node (pval, pleft, pright) ->
					if ((pval > nval) = exptd)
					&& ((height nodeleft) = (height noderight))
					&& (pleft <> Nil && pright <> Nil) then
						(loop ~parent:node nodeleft && loop ~parent:node ~exptd:false noderight)
					else false
				| Nil -> (loop ~parent:node nodeleft && loop ~parent:node ~exptd:false noderight))
		| Nil -> true
	in
	loop root

let is_balanced root =
	let rec loop node = match node with
		| Node (nval, nodeleft, noderight) ->
			if loop nodeleft && loop noderight && ((abs ((height nodeleft) - (height noderight))) <= 1) then true
			else false
		| Nil -> true
	in
	loop root

let search_bst root value =
	let rec loop node = match node with
		| Node (nval, nodeleft, noderight) ->
			if (value = nval) then true
			else if (value < nval) then loop (nodeleft)
			else loop (noderight)
		| Nil -> false
	in
	loop root

let main () =

	let last = Node (7, Nil, Nil) in

	let n1 = Node (8, last, Nil) in
	let n2 = Node (2, Nil, Nil) in
	let n3 = Node (4, Nil, Nil) in

	let m2 = Node (10, n1, Nil) in
	let m1 = Node (3, n2, n3) in

	let bst_notperfect = Node (5, m1, m2) in

	let perfect = Node (
			10,
			Node (
				5,
				Node (2, Nil, Nil),
				Node (7, Nil, Nil)
			),
			Node (
				15,
				Node (12, Nil, Nil),
				Node (17, Nil, Nil)
			)
		) in
	let wrong = Node (
			10,
			Node (
				5,
				Node (2222, Nil, Nil),
				Node (7, Nil, Nil)
			),
			Node (
				15,
				Node (12, Nil, Nil),
				Node (17, Nil, Nil)
			)
		) in

	print_endline "test bst";
	if (is_bst wrong) then print_endline "L'arbre wrong est juste ? damn..." else print_endline "L'arbre wrong est bien faux !";
	if (is_bst bst_notperfect) then print_endline "L'arbre bst_notperfect est bien bst !" else print_endline "erreur";
	if (is_bst perfect) then print_endline "L'arbre perfect est bien bst !" else print_endline "erreur";

	print_endline "\ntest perfect";
	if (is_perfect wrong) then print_endline "L'arbre wrong est juste ? damn..." else print_endline "L'arbre wrong est bien faux !";
	if (is_perfect last) then print_endline "last est perfect, narmol c'est un seul noeud" else print_endline "probleme !";
	if (is_perfect bst_notperfect) then print_endline "probleme!" else print_endline "L'arbre bst_notperfect n'est pas parfais, c'est cool !";
	if (is_perfect perfect) then print_endline "L'arbre perfect est bien parfais !" else print_endline "erreur";

	print_endline "\ntest balanced";
	if (is_balanced wrong) then print_endline "L'arbre wrong est balanced . damn..." else print_endline "L'arbre wrong faux ? c'est une erreur";
	if (is_balanced last) then print_endline "last est balanced, narmol c'est un seul noeud" else print_endline "probleme !";
	if (is_balanced bst_notperfect) then print_endline "probleme!" else print_endline "L'arbre bst_notperfect n'est pas balanced, c'est cool !";
	if (is_balanced perfect) then print_endline "L'arbre balanced est bien parfais !" else print_endline "erreur";

	print_endline "\ntest find";
	if (search_bst wrong 2222) then print_endline "a trouvé" else print_endline "L'arbre wrong n'apas trouvé 2222 ? c'est normal il n'est pas BST";
	if (search_bst last 2) then print_endline "erreur" else print_endline "nada, c'est bien !";
	if (search_bst bst_notperfect 8) then print_endline "trouvé, good !" else print_endline "erreur";
	if (search_bst perfect 17) then print_endline "trouvé, good !" else print_endline "erreur"



let () = main ()