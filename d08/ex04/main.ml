let main () =
	let x = new Reaction.alkane_combustion [(new Molecule.hydrogenGas, 2); (new Molecule.oxygen, 1)] [(new Molecule.water, 2)] in
	if (x#is_balanced) then print_endline "Is balanced" else print_endline "is NOT balanced";
	let inl = x#get_start
	and oul = x#get_result in
	print_endline "~~~~~~~~~~~~~~~~~~~~~~~~~~~IN :";
	List.iter (fun (item : Molecule.molecule * int) -> print_endline ((fst item)#to_string)) inl;
	print_endline "~~~~~~~~~~~~~~~~~~~~~~~~~~OUT :";
	List.iter (fun (item : Molecule.molecule * int) -> print_endline ((fst item)#to_string)) oul

let () = ignore @@ main ()