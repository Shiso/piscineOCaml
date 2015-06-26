class virtual reaction (inlst:(Molecule.molecule * int) list) (outlst:(Molecule.molecule * int) list) =
	object (self)
		val _start = inlst;
		val _result = outlst;

		method virtual get_start : (Molecule.molecule * int) list
		method virtual get_result : (Molecule.molecule * int) list

		method virtual balance : reaction
		method virtual is_balanced : bool

	end
