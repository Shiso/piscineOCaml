let repeat_string ?(str = "x") nb =
	if (nb < 0) then "Error"
	else begin
		let rec loop str nb =
			if (nb > 0)
			then str ^ loop str (nb - 1)
			else "";
		in
		loop str nb
	end



let main () =
	print_string("[5]:") ; print_endline(repeat_string(5));
	print_string("[42]:") ; print_endline(repeat_string(42));
	print_string("[0]:") ; print_endline(repeat_string(0));
	print_string("[-6]:") ; print_endline(repeat_string(-6));

	print_string("yolo [2]:") ; print_endline(repeat_string ~str:"yolo " 2);
	print_string("a[42]:") ; print_endline(repeat_string ~str:"a" 42);
	print_string("yomama[0]:") ; print_endline(repeat_string ~str:"yomama" 0);
	print_string("blabla[-6]:") ; print_endline(repeat_string ~str:"blabla" (-6))



let () = main ()