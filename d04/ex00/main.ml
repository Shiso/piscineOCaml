let main () =
	let l = Color.all in
	let rec loop lst = match lst with
		| hd::tl -> print_string (Color.toString hd) ; print_string " : " ; print_endline (Color.toStringVerbose hd); loop tl
		| [] -> print_endline "fin"
	in
	loop l

let () = main ()