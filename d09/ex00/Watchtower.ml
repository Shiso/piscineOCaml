module type WATCHTOWER =
	sig
		type hour = int
		val zero : hour
		val add : hour -> hour -> hour
		val sub : hour -> hour -> hour
		val print_hour : hour -> unit
	end

module Watchtower : WATCHTOWER =
	struct
		type hour = int
		let zero : hour = 0
		let add (x:hour) (y:hour) : hour = (x + y) mod 12

		let sub (x:hour) (y:hour) : hour =
				let rec loop res = match res with
					| n when n > 0 -> res
					| n -> loop (res + 12)
				in
				loop (x - y) mod 12

		let print_hour (x:hour) = print_string ((string_of_int x)^" ")

	end