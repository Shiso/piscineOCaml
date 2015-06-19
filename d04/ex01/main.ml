let main () =
	let l = Value.all in
	let rec loop lst = match lst with
		| hd::tl -> print_string (Value.toString hd) ; print_string " : " ; print_endline (Value.toStringVerbose hd); loop tl
		| [] -> print_endline "fin"
	in
	loop l;

	print_endline (Value.toStringVerbose (Value.next Value.Queen));
	(*print_endline (Value.toStringVerbose (Value.next Value.As));*)
	print_endline (Value.toStringVerbose (Value.previous Value.Queen))
	(*print_endline (Value.toStringVerbose (Value.previous Value.T2))*)

let () = main ()