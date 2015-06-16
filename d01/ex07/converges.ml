let rec converges f x n =
	if (n < 0) then false
	else if (x = f(x)) then true
	else if (n = 0) then false
	else converges f (f x) (n - 1)



let main () =
 print_endline "function for tests : (( * ) 2)";
 print_string "converges 5 -42 = "; print_string (string_of_bool (converges (( * ) 2) 5 (-42))); print_char '\n';
 print_string "converges 2 5 = "; print_string (string_of_bool (converges (( * ) 2) 2 5)); print_char '\n';
 print_string "converges 2 3 = "; print_string (string_of_bool (converges (( * ) 2) 2 3)); print_char '\n';

 print_endline "function for tests : (fun x -> x / 2)";
 print_string "converges 5 -42 = "; print_string (string_of_bool (converges (fun x -> x / 2) 5 (-42))); print_char '\n';
 print_string "converges 2 2 = "; print_string (string_of_bool (converges (fun x -> x / 2) 2 2)); print_char '\n';
 print_string "converges 2 3 = "; print_string (string_of_bool (converges (fun x -> x / 2) 2 3)); print_char '\n';
 print_string "converges 2 5 = "; print_string (string_of_bool (converges (fun x -> x / 2) 2 5)); print_char '\n';
 print_string "converges 4 2 = "; print_string (string_of_bool (converges (fun x -> x / 2) 4 2)); print_char '\n'

let () = main ()