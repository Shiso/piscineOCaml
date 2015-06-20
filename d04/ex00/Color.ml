module Color :
sig

	type t = Spade | Heart | Diamond | Club
	val all : t list
	val toString : t -> string
	val toStringVerbose : t -> string

end = struct

	type t = Spade | Heart | Diamond | Club

	let all = [Spade; Heart; Diamond; Club]

	let toString t =
		if (t = Spade) then "S"
		else if (t = Heart) then "H"
		else if (t = Diamond) then "D"
		else "C"

	let toStringVerbose t =
		if (t = Spade) then "Spade"
		else if (t = Heart) then "Heart"
		else if (t = Diamond) then "Diamond"
		else "Club"
end


