let main () =
	let molecules : Molecule.molecule list = [
		new Molecule.carbonDioxyde;
		new Molecule.trinitrotoluene;
		new Molecule.water;
		new Molecule.aconitine;
		new Molecule.tetrahydrocannabinol;
		new Molecule.cisplatin;
	] in
	List.iter (fun (x:Molecule.molecule) -> print_endline (x#get_formula)) molecules

let () = ignore @@ main ()