let rec iter f x n =
	if (n < 0) then -1
	else if (n = 0) then x
	else f (iter f x (n - 1))



let main () =
	print_endline "function for tests : (fun x -> x * x)";
	print_string "iter 5 -42="; print_int (iter (fun x -> x * x) 5 (-42)); print_char '\n';
	print_string "iter 2 2="; print_int (iter (fun x -> x * x) 2 2); print_char '\n';
	print_string "iter 2 4="; print_int (iter (fun x -> x * x) 2 4); print_char '\n';
	print_endline "function for tests : (fun x -> x * 2)";
	print_string "iter 5 -42="; print_int (iter (fun x -> x * 2) 5 (-42)); print_char '\n';
	print_string "iter 2 2="; print_int (iter (fun x -> x * 2) 2 2); print_char '\n';
	print_string "iter 2 4="; print_int (iter (fun x -> x * 2) 2 4); print_char '\n'

let () = main ()