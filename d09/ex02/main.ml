include Calc


module Calc_int = Calc (MonoidInt)
module Calc_float = Calc (MonoidFloat)

let main () =
	print_endline (string_of_int (Calc_int.power 3 3));
	print_endline (string_of_float (Calc_float.power 3.0 3));
	print_endline (string_of_int (Calc_int.mul (Calc_int.add 20 1) 2));
	print_endline (string_of_float (Calc_float.mul (Calc_float.add 20.0 1.0) 2.0));
	print_endline (string_of_int (Calc_int.fact (Calc_int.sub 5 1)));
	print_endline (string_of_float (Calc_float.fact (Calc_float.sub 5.0 1.0)));
	print_endline (string_of_int (Calc_int.fact (Calc_int.div 10 2)));
	print_endline (string_of_float (Calc_float.fact (Calc_float.div 10.0 2.0)));
	print_endline (string_of_int (Calc_int.add (Calc_int.mul 10 2) 2));
	print_endline (string_of_float (Calc_float.add (Calc_float.mul 10.0 2.0) 2.0))

let () = main ()