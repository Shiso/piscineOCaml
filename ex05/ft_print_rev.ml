
let ft_print_rev text =
	let len = String.length text in
	let rec loop len =
		if (len >= 0) then begin
			print_char (String.get text len);
			loop (len - 1)
		end
	in
	loop (len - 1);
	print_char('\n')

let param = Sys.argv.(1)
let () = ft_print_rev param