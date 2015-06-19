module type Value =
sig
	type t

	(** The list of all values of type t *)
	val all : t list
	(** Interger representation of a card value, from 1 for T2 to 13 for As *)
	val toInt : t -> int
	(** returns "2", ..., "10", "J", "Q", "K" or "A" *)
	val toString : t -> string
	(** returns "2", ..., "10", "Jack", "Queen", "King" or "As" *)
	val toStringVerbose : t -> string
	(** Returns the next value, or calls invalid_arg if argument is As *)
	val next : t -> t
	(** Returns the previous value, or calls invalid_arg if argument is T2 *)
	val previous : t -> t
end
	type t = T2 | T3 | T4 | T5 | T6 | T7 | T8 | T9 | T10 | Jack | Queen | King | As

	let all = [T2; T3; T4; T5; T6; T7; T8; T9; T10; Jack; Queen; King; As]

	let toInt t =
		let lst = all in
		let rec loop ?(value=1) liste = match liste with
			| hd::tl -> if t <> hd then loop ~value:(value + 1) tl else value
			| [] -> -42
		in
		loop lst

	let next t =
		if t = T2 then T3
		else if t = T3 then T4
		else if t = T4 then T5
		else if t = T5 then T6
		else if t = T6 then T7
		else if t = T7 then T8
		else if t = T8 then T9
		else if t = T9 then T10
		else if t = T10 then Jack
		else if t = Jack then Queen
		else if t = Queen then King
		else if t = King then As
		else invalid_arg "As invalid"

	let previous t =
		if t = T2 then invalid_arg "T2 invalid"
		else if t = T3 then T2
		else if t = T4 then T3
		else if t = T5 then T4
		else if t = T6 then T5
		else if t = T7 then T6
		else if t = T8 then T7
		else if t = T9 then T8
		else if t = T10 then T9
		else if t = Jack then T10
		else if t = Queen then Jack
		else if t = King then Queen
		else King

	let toString t =
		if t = T2 then "2"
		else if t = T3 then "3"
		else if t = T4 then "4"
		else if t = T5 then "5"
		else if t = T6 then "6"
		else if t = T7 then "7"
		else if t = T8 then "8"
		else if t = T9 then "9"
		else if t = T10 then "10"
		else if t = Jack then "J"
		else if t = Queen then "Q"
		else if t = King then "K"
		else "A"

	let toStringVerbose t =
		if t = T2 then "2"
		else if t = T3 then "3"
		else if t = T4 then "4"
		else if t = T5 then "5"
		else if t = T6 then "6"
		else if t = T7 then "7"
		else if t = T8 then "8"
		else if t = T9 then "9"
		else if t = T10 then "10"
		else if t = Jack then "Jack"
		else if t = Queen then "Queen"
		else if t = King then "King"
		else "As"