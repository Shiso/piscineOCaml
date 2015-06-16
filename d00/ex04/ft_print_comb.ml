
let ft_print_comb () =
	let start = 0 in
	let rec loopx x =
		if x <= 7 then
		begin
			let rec loopy y =
				if y <= 8 then
				begin
					let rec loopz z =
						if z <= 9 then
						begin
							print_int x;
							print_int y;
							print_int z;
							if (x = 7)
							then print_string "\n"
							else print_string ", ";
							loopz (z + 1)
						end
					in
					loopz (y + 1);
					loopy (y + 1)
				end
			in
			loopy (x + 1);
			loopx (x + 1)
		end
	in
	loopx start

let () = ft_print_comb ()