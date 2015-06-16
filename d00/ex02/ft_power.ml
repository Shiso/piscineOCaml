let rec ft_power nb exp =
	if (exp <= 0) then 1
	else
		let mult = ft_power nb (exp / 2) in
		if ((exp mod 2) = 0) then mult * mult
		else nb * mult * mult

let param1 = int_of_string Sys.argv.(1)
let param2 = int_of_string Sys.argv.(2)
let () = print_int (ft_power param1 param2);
print_char '\n'