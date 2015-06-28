class obj =
	object (self)
		val mutable _health = 100
		val mutable _energy = 100
		val mutable _hygiene = 100
		val mutable _happyness = 100

		method private add x y =
			if (x + y) > 100 then 100
			else if (x + y) <= 0 then failwith "Game Over"
			else x + y

		method get_health = _health
		method get_energy = _energy
		method get_hygiene = _hygiene
		method get_happyness = _happyness
		method check_alive = (_health > 0) && (_energy > 0) && (_hygiene > 0) && (_happyness > 0)
		(* method eat = {< _health = (self#add _health 25); _energy = (self#add _energy (-10)); _hygiene = (self#add _hygiene (-20)); _happyness = (self#add _happyness 5) >}
		method thunder = {< _health = (self#add _health (-20)); _energy = (self#add _energy 25); _happyness = (self#add _happyness (-20)) >}
		method bath = {< _health = (self#add _health (-20)); _energy = (self#add _energy (-10)); _hygiene = (self#add _hygiene 25); _happyness = (self#add _happyness 5) >}
		method kill = {< _health = (self#add _health (-20)); _energy = (self#add _energy (-10)); _happyness = (self#add _happyness 20) >}
		method tic = {< _health = (self#add _health (-1)); _energy = _energy; _hygiene = _hygiene; _happyness = _happyness >}
 *)
		method eat = _health <- self#add _health 25; _energy <- self#add _energy (-10); _hygiene <- self#add _hygiene (-20); _happyness <- self#add _happyness 5
		method thunder = _health <- self#add _health (-20); _energy <- self#add _energy 25; _happyness <- self#add _happyness (-20)
		method bath = _health <- self#add _health (-20); _energy <- self#add _energy (-10); _hygiene <- self#add _hygiene 25; _happyness <- self#add _happyness 5
		method kill = _health <- self#add _health (-20); _energy <- self#add _energy (-10); _happyness <- self#add _happyness 20
		method hit = _health <- self#add _health (-10); _happyness <- self#add _happyness (-30)
		method tic = _health <- self#add _health (-1)
		method reset = _health <- 100;_energy <- 100;_hygiene <- 100;_happyness <- 100
		method state = (_health < 50 || _energy < 50 ||  _hygiene < 50 || _happyness < 50)

		method load =
			let savefile = open_in "save.itama" in
			let l1 = input_line savefile
			and l2 = input_line savefile
			and l3 = input_line savefile
			and l4 = input_line savefile in
			close_in savefile;
			{< 
				_health = int_of_string l1;
				_energy = int_of_string l2;
				_hygiene = int_of_string l3;
				_happyness = int_of_string l4
			>}

		method save =
			let savefile = open_out "save.itama" in
			output_string savefile	((string_of_int (self#get_health))^"\n"^
									(string_of_int (self#get_energy))^"\n"^
									(string_of_int (self#get_hygiene))^"\n"^
									(string_of_int (self#get_happyness)));
			close_out savefile
	end

(* let get_game () =
	let o = new obj in
	try let o1 = o#load in o1 with _ -> o

let () =
	let o = get_game () in
	let o = try o#kill with Failure s ->
							let savefile = open_out "save.itama" in
							close_out savefile;
							print_endline (s);new obj
	in
	print_endline (string_of_int o#get_health)
	;print_endline (string_of_int o#get_energy)
	;print_endline (string_of_int o#get_hygiene)
	;print_endline (string_of_int o#get_happyness)
	;o#save *)