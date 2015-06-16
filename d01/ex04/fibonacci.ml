let rec fibonacci n =
	if (n < 0) then -1
	else if (n <=1) then n
	else begin
		let rec loop n a b =
			if n = 0 then a
			else loop (n-1) b (a+b)
		in
		loop n 0 1
	end

let main () =
	print_string "-42="; print_int (fibonacci (-42)); print_char '\n';
	print_string "1="; print_int (fibonacci 1); print_char '\n';
	print_string "3="; print_int (fibonacci 3); print_char '\n';
	print_string "6="; print_int (fibonacci 6); print_char '\n';
	print_string "12="; print_int (fibonacci 12); print_char '\n'


let () = main ()