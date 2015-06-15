let is_digit c = c >= '0' && c <= '9'
let is_upper c = c >= 'A' && c <= 'Z'
let is_lower c = c >= 'a' && c <= 'z'

let ft_string_all func text =
	let len = String.length text in
	let rec loop current =
		if (current > (len - 1)) then true
		else begin
			if (func(String.get text current))
			then loop(current + 1)
			else false
		end
	in
	loop (0)

let main () =
	let param = Sys.argv.(1) in
	if (ft_string_all is_digit param)
	then print_endline "Valid"
	else print_endline "Invalid"

let () = main ()