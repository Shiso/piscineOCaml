
let sum a b = a +. b

let main () =
	print_string "sim of 1. + 8. = " ; print_float (sum 1. 8.); print_endline "";
	print_string "sim of -42.2 + 8.2 = " ; print_float (sum (-45.) 8.); print_endline "";
	print_string "sim of Nan + 8. = " ; print_float (sum nan 8.); print_endline "";
	print_string "sim of 1. + neg_infinity = " ; print_float (sum 1. neg_infinity); print_endline "";
	print_string "sim of 1. + infinity = " ; print_float (sum 1. infinity); print_endline "";
	print_string "sim of 1. + 8. = " ; print_float (sum 1. 8.); print_endline ""

let () = ignore (main ())