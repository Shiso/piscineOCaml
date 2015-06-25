let atomlist n = 
	let res = [new Atom.hydrogen;new Atom.hydrogen;] in
	let rec loop res = function
		| 0 -> res
		| n -> loop (res @ [new Atom.carbon;new Atom.hydrogen;new Atom.hydrogen;]) (n - 1)
	in
	loop res n

class alkane n =
	object (self)
		inherit Molecule.molecule "Alkane" (atomlist n)

		equals
	end