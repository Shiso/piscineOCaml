type sidek = None | Companion of People.people

class doctor name =
	object (self)

		val _name : string = name
		val mutable _age = 0
		val mutable _hp = 100
		val mutable _sidekick : sidek = None

		initializer print_endline @@ ("A Doctor is born ! : " ^ self#to_string)

		method to_string = match _sidekick with
		| None -> _name ^" [doctor] "^string_of_int (_age) ^" years old, "^string_of_int(_hp)^"hp."
		| Companion f -> _name ^" [doctor] "^string_of_int (_age) ^" years old, "^string_of_int(_hp)^"hp, accompanied by "^f#to_string

		method talk = print_endline "Hi! I’m the Doctor!"
		method die = print_endline "Aaaarghh!"

		method private regenerate = _hp <- 100
		method use_sonic_screwdriver = print_endline "Whiiiiwhiiiwhiii Whiiiiwhiiiwhiii Whiiiiwhiiiwhiii" 
		method travel_in_time (start:int) (arrival:int) =
			print_endline "
          _
         /-\\
    _____|#|_____
   |_____________|
  |_______________|
|||_POLICE_##_BOX_|||
 | |¯|¯|¯|||¯|¯|¯| |
 | |-|-|-|||-|-|-| |
 | |_|_|_|||_|_|_| |
 | ||~~~| | |¯¯¯|| |
 | ||~~~|!|!| O || |
 | ||~~~| |.|___|| |
 | ||¯¯¯| | |¯¯¯|| |
 | ||   | | |   || |
 | ||___| | |___|| |
 | ||¯¯¯| | |¯¯¯|| |
 | ||   | | |   || |
 | ||___| | |___|| |
|¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯|
 ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯"
			

		method get_name = _name
		method get_age = _age
		method get_hp = _hp
		method get_sidekick = _sidekick

		method test_regenerate = self#regenerate
		method set_age x = _age <- x
		method set_hp x = _hp <- x
		method set_sidekick (sk : People.people) = _sidekick <- Companion sk

	end
