let main () =
	let r1 = new Reaction.water_balanced in
	if (r1#is_balanced) then print_endline "R1 balanced" else print_endline "unbalanced reaction"

let () = ignore @@ main ()