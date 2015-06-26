class virtual reaction (inlst:(Molecule.molecule * int) list) (outlst:(Molecule.molecule * int) list) =
	object (self)
		val _start = inlst;
		val _result = outlst;

		method get_start = _start
		method get_result = _result

		method is_balanced =
			let rec get_atoms_list res (lst:(Molecule.molecule * int) list) = match lst with
				| [] -> res
				| h::t -> let rec loop res = (function
								| 0 -> res
								| x -> loop (res @ ((fst h)#get_atoms)) (x - 1))
							in
							get_atoms_list (loop res (snd h)) t
			in
			let inAtoms = get_atoms_list [] _start
			and outAtoms = get_atoms_list [] _result in
			let sortedIn = List.sort (fun h t -> (compare (h#get_symbol) (t#get_symbol))) inAtoms
			and sortedOut = List.sort (fun h t -> (compare (h#get_symbol) (t#get_symbol))) outAtoms in
			let rec compare (inl : Atom.atom list) (out : Atom.atom list) = match inl, out with
				| [], [] -> true
				| _ , []-> false
				| [], _ -> false
				| h1::t1, h2::t2 -> if (h1#get_symbol) = (h2#get_symbol) then compare t1 t2 else false
			in
			compare sortedIn sortedOut

	end

class water_balanced =
	object (self)
		inherit reaction [(new Molecule.hydrogenGas, 2); (new Molecule.oxygen, 1)] [(new Molecule.water, 2)]
	end