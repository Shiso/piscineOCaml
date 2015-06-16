let rec repeat_x nb =
	if (nb < 0) then "Error"
	else begin
		if (nb > 0) then "x" ^ repeat_x(nb - 1)
		else ""
	end





let main () =
	print_string("[5]:") ; print_endline(repeat_x(5));
	print_string("[42]:") ; print_endline(repeat_x(42));
	print_string("[0]:") ; print_endline(repeat_x(0));
	print_string("[-6]:") ; print_endline(repeat_x(-6))



let () = main ()