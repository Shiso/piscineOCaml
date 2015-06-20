
module Value :
sig

	type t = T2 | T3 | T4 | T5 | T6 | T7 | T8 | T9 | T10 | Jack | Queen | King | As
	val all : t list
	val toInt : t -> int
	val toString : t -> string
	val toStringVerbose : t -> string
	val next : t -> t
	val previous : t -> t

end = struct

	type t = T2 | T3 | T4 | T5 | T6 | T7 | T8 | T9 | T10 | Jack | Queen | King | As

	let all = [T2; T3; T4; T5; T6; T7; T8; T9; T10; Jack; Queen; King; As]

	let toInt t =
		let lst = all in
		let rec loop ?(value=1) liste = match liste with
			| hd::tl -> if t <> hd then loop ~value:(value + 1) tl else value
			| [] -> -42
		in
		loop lst

	let next t = match t with
		| T2 -> T3
		| T3 -> T4
		| T4 -> T5
		| T5 -> T6
		| T6 -> T7
		| T7 -> T8
		| T8 -> T9
		| T9 -> T10
		| T10 -> Jack
		| Jack -> Queen
		| Queen -> King
		| King -> As
		| _ -> invalid_arg "As invalid"

	let previous t = match t with
		| T2 -> invalid_arg "T2 invalid"
		| T3 -> T2
		| T4 -> T3
		| T5 -> T4
		| T6 -> T5
		| T7 -> T6
		| T8 -> T7
		| T9 -> T8
		| T10 -> T9
		| Jack -> T10
		| Queen -> Jack
		| _ -> King

	let toString t = match t with
		| T2 -> "2"
		| T3 -> "3"
		| T4 -> "4"
		| T5 -> "5"
		| T6 -> "6"
		| T7 -> "7"
		| T8 -> "8"
		| T9 -> "9"
		| T10 -> "10"
		| Jack -> "J"
		| Queen -> "Q"
		| King -> "K"
		| _ -> "A"

	let toStringVerbose t = match t with
		| T2 -> "2"
		| T3 -> "3"
		| T4 -> "4"
		| T5 -> "5"
		| T6 -> "6"
		| T7 -> "7"
		| T8 -> "8"
		| T9 -> "9"
		| T10 -> "10"
		| Jack -> "Jack"
		| Queen -> "Queen"
		| King -> "King"
		| _ -> "As"
end
