let main () =
	let molecules : Molecule.molecule list = [
		new Molecule.carbonDioxyde;
		new Molecule.trinitrotoluene;
		new Molecule.water;
		new Molecule.aconitine;
		new Molecule.tetrahydrocannabinol;
		new Molecule.cisplatin;
	] in
	List.iter (fun (x:Molecule.molecule) -> print_endline (x#to_string)) molecules;
	let c1 = new Molecule.cisplatin
	and c2 = new Molecule.cisplatin in
	if (c1#equals c2) then print_endline "molecules de cisplatin identiques" else print_endline "error"
let () = ignore @@ main ()