module People = struct
	class people name =
		object (self)
			val _name : string = name
			val mutable _hp = 100

			initializer print_endline @@ ("a people as join the party : " ^ self#to_string)

			method to_string = _name ^" [people] "^string_of_int (_hp)^"hp"
			method talk = print_endline @@ "I’m "^ _name ^"! Do you know the Doctor?"
			method die = print_endline "Aaaarghh!"
		end
end