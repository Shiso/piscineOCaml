let rec ft_countdown nb =
	if nb < 0
	then print_int (0)
	else print_int (nb);
	print_char '\n';
	if (nb > 0) then ft_countdown (nb - 1)


let param = int_of_string Sys.argv.(1)
let () = ft_countdown (param)