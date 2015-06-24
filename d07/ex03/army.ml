class ['a] army =
	object (self)

		val mutable members : 'a list = []

		initializer print_endline "new army created"

		method add (item : 'a) = members <- members @ [item]

		method remove = members <- match members with | [] -> members | ab::ris -> ris

		method get = members
		method set lst = members <- lst 


	end