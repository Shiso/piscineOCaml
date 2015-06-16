let ft_sum f min max =
	let	rec loop i res =
		if (i > max) then res
		else loop (i + 1) ((f i) +. res)
	in
	loop min 0.0


let main () =
 print_endline "function for tests : (fun i -> float_of_int (i * i)) ";

 print_string "ft_sum 1 10 = ";
 print_float (ft_sum (fun i -> float_of_int (i * i)) 1 10); print_char '\n';
 print_string "ft_sum 3 6 = ";
 print_float (ft_sum (fun i -> float_of_int (i * i)) 3 6); print_char '\n'

let () = main ()