let rec get_repeat ?(res=0) liste item = match liste with
	| head::tail ->
		(* print_string " progressive res" ; print_int(res); *)
		if (head <> item)
			then res
		else get_repeat ~res:(res + 1) tail item
	| [] -> res

(* type ('a) t = int * 'a *)

let encode ?(rlist = []) liste =
	if (liste = []) then []
	else begin
		let l = liste in
		let rec loop res_list liste i = match liste with
		| head::tail ->
			if (i = 0) then
				let nb_iter = get_repeat liste head in
				loop (res_list @ [(nb_iter, head)]) tail (nb_iter - 1)
			else loop res_list tail (i - 1)
		| [] -> res_list
		in
		loop rlist l 0
	end


let rec print_list_int = function
	| head::tail ->
		print_int (head); print_string " "; print_list_int tail
	| [] -> print_char '\n'
	| [] -> print_char '\n'

let rec print_list_string = function
	| head::tail ->
		print_string (head); print_string " "; print_list_string tail
	| [] -> print_char '\n'

let rec print_list_int_res = function
	| head::tail ->
		let (nb, elem) = head in
		print_int (nb); print_char '.'; print_int (elem); print_char ' '; print_list_int_res tail
	| [] -> print_char '\n'

let rec print_list_string_res = function
	| head::tail ->
		let (nb, elem) = head in
		print_int (nb); print_char '.'; print_string (elem); print_char ' '; print_list_string_res tail
	| [] -> print_char '\n'

let main () =
	let liste1 = [2;2;2;2;2;3;3;3;4;2;1;2;4;4;2;2;2;1;1;1] in
	let liste2 = ["toto";"tata";"tata";"toto";"toto";"toto";"tata";"tata";"toto";] in
	print_string "liste d'int : " ; print_list_int liste1; print_char '\n';
	print_string "liste de string : " ; print_list_string liste2; print_char '\n';
	let resvide = encode [] in
	let res = encode liste1 in
	let res2 = encode liste2 in
	print_string "Resultat liste vide : " ; print_list_int_res resvide; print_char '\n';
	print_string "Resultat liste d'int : " ; print_list_int_res res; print_char '\n';
	print_string "Resultat liste de string : " ; print_list_string_res res2; print_char '\n'


let () = main ()