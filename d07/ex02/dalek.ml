let gen caps = match Random.int 26 with
	| n when caps = false -> int_of_char 'a' + n
	| n -> int_of_char 'A' + n

class dalek =
	object (self)
		
		val _name : string = Random.self_init (); "Darlek" ^ ((String.make 1 (char_of_int (gen true))) ^ (String.make 1 (char_of_int (gen false))) ^ (String.make 1 (char_of_int (gen false))))
		val mutable _hp = 100
		val mutable _shield = true
		val phrases = [|"Explain! Explain!"; "Exterminate! Exterminate!"; "I obey!"; "You are the Doctor! You are the enemy of the Daleks!"|]

		initializer print_endline @@ ("a dalek as join the empire! : " ^ self#to_string)


		method to_string = _name ^" [dalek] "^string_of_int (_hp)^"hp, shield "^(self#get_shield_verbose ())
		method talk = print_endline @@ phrases.(Random.int 4)
		method die = print_endline "Emergency Temporal Shift!"

		method exterminate (target:People.people) = _shield <- (not _shield); target#die

		method get_name = _name
		method get_hp = _hp

		method get_shield = _shield
		method get_shield_verbose () = if (_shield) then "UP" else "DOWN"
	end
