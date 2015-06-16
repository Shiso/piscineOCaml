
let ft_is_palindrome text =
	let len = String.length text in
	let rec loop current =
		if (current < (((len) / 2) + (len mod 2))) then begin
			if (String.get text current == String.get text (len - 1 - current))
			then loop (current + 1)
			else false
		end
		else true
	in
	loop 0

let main () =
	let param = Sys.argv.(1) in
	if (ft_is_palindrome param)
	then print_endline "Valid"
	else print_endline "Invalid"

let () = main ()