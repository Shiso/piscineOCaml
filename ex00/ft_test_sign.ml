
let ft_test_sign nb =
	if (nb < 0)
	then print_endline "negative"
	else print_endline "positive"


let param = int_of_string Sys.argv.(1)
let () = ft_test_sign param