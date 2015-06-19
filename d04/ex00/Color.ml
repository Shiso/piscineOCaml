module type Color =
sig
	type t

	val all : t list (** The list of all values of type t *)
	val toString : t -> string (** "S", "H", "D" or "C" *)
	val toStringVerbose : t -> string (** "Spade", "Heart", etc *)

end
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