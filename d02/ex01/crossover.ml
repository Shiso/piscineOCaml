let rec crossover ?(res=[]) lst1 lst2 = 
	if (lst2 = []) then []
	else begin
		match lst1 with
		| hd1::tl1 ->
			let rec check_added resu = match resu with
			| hdr::tlr ->
				if (hd1 = hdr) then crossover ~res:res tl1 lst2
				else check_added tlr
			| [] ->
				let rec check_second_list list2 = match list2 with
				| hd2::tl2 ->
					if (hd2 = hd1) then crossover ~res:(res @ [hd2]) tl1 lst2
					else check_second_list tl2
				| [] -> crossover ~res:res tl1 lst2
				in
				check_second_list lst2
			in
			check_added res
		| [] -> res
	end


let rec print_list_int = function
	| head::tail ->
		print_int (head); print_string " "; print_list_int tail
	| [] -> print_char '\n'

let rec print_list_string = function
	| head::tail ->
		print_string (head); print_string " "; print_list_string tail
	| [] -> print_char '\n'


let main () =
	let liste1 = [2;2;2;2;2;3;3;3;4;2;1;2;4;4;2;2;2;1;1;1] in
	let liste2 = [2;2;1;1;1;3;3;3;4;2;1;2;4;4;2;2;2;1;1;1] in

	print_list_int liste1;
	print_list_int liste2;
	print_list_int (crossover liste1 liste2)

let () = main ()