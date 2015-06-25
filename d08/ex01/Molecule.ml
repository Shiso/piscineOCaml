let makeStr lst =
	let rec loop_nb item nb lst = match lst with
		| [] -> (nb, [])
		| h::t -> if (h#get_symbol = item#get_symbol) then loop_nb item (nb + 1) t else (nb, lst)
	in
	let rec loop str = function
		| [] -> str
		| h::t ->
			let iter = loop_nb h 1 t in
			(match iter with
				| (1, lst) -> loop (str ^ h#get_symbol) t
				| (n, lst) -> loop (str ^ h#get_symbol ^ (string_of_int n)) lst)
	in
	loop "" lst

let init_formula (atoms:Atom.atom list) =
	let ordered_table = [] in
	let rec loop otable = function
		| [] -> otable
		| h::t -> if (h#get_symbol = "C") then loop ([h] @ otable) t else loop otable t
	in
	let ordered_table = loop ordered_table atoms in
	if (List.length ordered_table = 0)
	then let result = List.sort (fun h t -> (compare (h#get_symbol) (t#get_symbol))) atoms in makeStr result
	else begin
		let rec populate otable = function
			| [] -> otable
			| h::t -> if (h#get_symbol = "H") then populate (otable @ [h]) t else populate otable t
		in
		let ordered_table = populate ordered_table atoms
		and filtered = List.filter (fun x -> (x#get_symbol <> "C") && (x#get_symbol <> "H")) atoms in
		let sorted = List.sort (fun h t -> (compare (h#get_symbol) (t#get_symbol))) filtered in
		let ordered_table = ordered_table @ sorted in makeStr ordered_table
	end

class virtual molecule name (atoms:Atom.atom list) =
	object (self)

		val _name : string = name
		val _formula : string = init_formula atoms

		method get_name : string = _name
		method get_formula : string = _formula

		method to_string : string = ((self#get_name)^(String.make (10 - (String.length (self#get_name))) ' ')^":  "^(self#get_formula))
	
	end

class carbonDioxyde =
	object
		inherit molecule "Carbon Dioxyde" [new Atom.hydrogen; new Atom.hydrogen; new Atom.oxygen]
	end

class water =
	object
		inherit molecule "Water" [new Atom.oxygen; new Atom.carbon; new Atom.oxygen]
	end

class trinitrotoluene =
	object
		inherit molecule "Trinitrotoluene" [new Atom.nitrogen;new Atom.hydrogen;new Atom.oxygen;new Atom.carbon;new Atom.nitrogen;new Atom.hydrogen;new Atom.oxygen;new Atom.carbon;new Atom.nitrogen;new Atom.hydrogen;new Atom.oxygen;new Atom.carbon;new Atom.carbon;new Atom.carbon;new Atom.oxygen;new Atom.oxygen;new Atom.oxygen;new Atom.carbon;new Atom.carbon;new Atom.hydrogen;new Atom.hydrogen;]
	end

class aconitine =
	object
		inherit molecule "Aconitine" [
		new Atom.carbon; new Atom.carbon; new Atom.carbon; new Atom.carbon; new Atom.carbon; new Atom.carbon; new Atom.carbon; new Atom.carbon; new Atom.carbon; new Atom.carbon; new Atom.carbon; new Atom.carbon;  new Atom.carbon; new Atom.carbon; new Atom.carbon; new Atom.carbon; new Atom.carbon; new Atom.carbon; new Atom.carbon; new Atom.carbon; new Atom.carbon; new Atom.carbon; new Atom.carbon;  new Atom.carbon; new Atom.carbon; new Atom.carbon; new Atom.carbon; new Atom.carbon; new Atom.carbon; new Atom.carbon; new Atom.carbon; new Atom.carbon; new Atom.carbon; new Atom.carbon;
		new Atom.hydrogen; new Atom.hydrogen; new Atom.hydrogen; new Atom.hydrogen; new Atom.hydrogen; new Atom.hydrogen; new Atom.hydrogen; new Atom.hydrogen; new Atom.hydrogen; new Atom.hydrogen; new Atom.hydrogen; new Atom.hydrogen; new Atom.hydrogen; new Atom.hydrogen; new Atom.hydrogen; new Atom.hydrogen; new Atom.hydrogen; new Atom.hydrogen; new Atom.hydrogen; new Atom.hydrogen; new Atom.hydrogen; new Atom.hydrogen; new Atom.hydrogen; new Atom.hydrogen; new Atom.hydrogen; new Atom.hydrogen; new Atom.hydrogen; new Atom.hydrogen; new Atom.hydrogen; new Atom.hydrogen; new Atom.hydrogen; new Atom.hydrogen; new Atom.hydrogen; new Atom.hydrogen; new Atom.hydrogen; new Atom.hydrogen; new Atom.hydrogen; new Atom.hydrogen; new Atom.hydrogen; new Atom.hydrogen; new Atom.hydrogen; new Atom.hydrogen; new Atom.hydrogen; new Atom.hydrogen; new Atom.hydrogen; new Atom.hydrogen; new Atom.hydrogen;
		new Atom.nitrogen;
		new Atom.oxygen; new Atom.oxygen; new Atom.oxygen; new Atom.oxygen; new Atom.oxygen; new Atom.oxygen; new Atom.oxygen; new Atom.oxygen; new Atom.oxygen; new Atom.oxygen; new Atom.oxygen;
		]
	end

class tetrahydrocannabinol =
	object
		inherit molecule "Tetrahydrocannabinol" [
		new Atom.carbon;new Atom.carbon;new Atom.carbon;new Atom.carbon;new Atom.carbon;new Atom.carbon;new Atom.carbon;new Atom.carbon;new Atom.carbon;new Atom.carbon;new Atom.carbon;new Atom.carbon;new Atom.carbon;new Atom.carbon;new Atom.carbon;new Atom.carbon;new Atom.carbon;new Atom.carbon;new Atom.carbon;new Atom.carbon;new Atom.carbon;
		new Atom.hydrogen;new Atom.hydrogen;new Atom.hydrogen;new Atom.hydrogen;new Atom.hydrogen;new Atom.hydrogen;new Atom.hydrogen;new Atom.hydrogen;new Atom.hydrogen;new Atom.hydrogen;new Atom.hydrogen;new Atom.hydrogen;new Atom.hydrogen;new Atom.hydrogen;new Atom.hydrogen;new Atom.hydrogen;new Atom.hydrogen;new Atom.hydrogen;new Atom.hydrogen;new Atom.hydrogen;new Atom.hydrogen;new Atom.hydrogen;new Atom.hydrogen;new Atom.hydrogen;new Atom.hydrogen;new Atom.hydrogen;new Atom.hydrogen;new Atom.hydrogen;new Atom.hydrogen;new Atom.hydrogen;
		new Atom.oxygen;new Atom.oxygen;
		]
	end

class cisplatin =
	object
		inherit molecule "Cisplatin" [
		new Atom.hydrogen;new Atom.hydrogen;new Atom.hydrogen;new Atom.hydrogen;new Atom.hydrogen;new Atom.hydrogen;
		new Atom.chlorine;new Atom.chlorine;
		new Atom.nitrogen;new Atom.nitrogen;
		new Atom.platinum;
		]
	end


