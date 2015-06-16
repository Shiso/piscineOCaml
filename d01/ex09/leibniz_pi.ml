let leibniz_pi delta =
	let rec loop i res =
		let f = (fun i -> ((float_of_int (1 - (2 * (i mod 2)))) /. (float_of_int ((2 * i) + 1)))) in
			let diff = ((4. *. (res +. (f i))) -. (4. *. (atan 1.))) in
			if (((diff < 0.) && (diff > (-.delta))) || ((diff > 0.) && (diff < delta)))
				then (4. *. ((f i) +. res))
			else
				loop (i + 1) ((f i) +. res)
	in
	loop 0 0.0

let main () =
 print_string "pi 0.5 =\n   ";
 print_float (leibniz_pi 0.5); print_char '\n';
 print_string "pi 0.001 =\n   ";
 print_float (leibniz_pi 0.001); print_char '\n';
 print_string "pi 0.000001 =\n   ";
 print_float (leibniz_pi 0.000001); print_char '\n'

let () = main ()