
let ft_print_alphabet () =
	let start = int_of_char 'a' in
	let end = int_of_char 'z' in
	let rec loop ascii_current_char =
		if ascii_current_char <= end then
		let current_char = char_of_int(ascii_current_char) in
		print_char current_char;
		loop (ascii_current_char + 1)
	in
	loop start;
	print_char '\n'

let () = ft_print_alphabet ()