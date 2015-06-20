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


module Card :
sig
	type t
	val newCard : Value.t -> Color.t -> t
	val allSpades : t list
 	val allHearts : t list
	val allDiamonds : t list
	val allClubs : t list
	val all : t list
	val getValue : t -> Value.t
	val getColor : t -> Color.t
	val toString : t -> string
	val toStringVerbose : t -> string
	val compare : t -> t -> int
	val max : t -> t -> t
	val min : t -> t -> t
	val best : t list -> t
	val isOf : t -> Color.t -> bool
	val isSpade : t -> bool
	val isHeart : t -> bool
	val isDiamond : t -> bool
	val isClub : t -> bool 
end = struct
	
	type t = {
			color : Color.t;
			value : Value.t;
		}

	let newCard v c = {
		color = c;
		value = v;
	}

	let getValue (item : t) = item.value
	let getColor (item : t) = item.color


	let allSpades = 
		let retlst : t list = [] in
		let rec loop ret_list lst = match lst with
			| hd::tl -> loop (ret_list @ [newCard hd Color.Spade]) tl
			| [] -> ret_list

		in loop retlst (Value.all)

	let allHearts = 
		let retlst : t list = [] in
		let rec loop ret_list lst = match lst with
			| hd::tl -> loop (ret_list @ [newCard hd Color.Heart]) tl
			| [] -> ret_list

		in loop retlst (Value.all)

	let allDiamonds = 
		let retlst : t list = [] in
		let rec loop ret_list lst = match lst with
			| hd::tl -> loop (ret_list @ [newCard hd Color.Diamond]) tl
			| [] -> ret_list

		in loop retlst (Value.all)

	let allClubs = 
		let retlst : t list = [] in
		let rec loop ret_list lst = match lst with
			| hd::tl -> loop (ret_list @ [newCard hd Color.Club]) tl
			| [] -> ret_list

		in loop retlst (Value.all)

	let all = allSpades @ allHearts @ allDiamonds @ allClubs


	let toString card = (Value.toString card.value) ^ (Color.toString card.color)
	let toStringVerbose card = "Card(" ^ (Value.toStringVerbose card.value) ^ ", " ^ (Color.toStringVerbose card.color)^ ")"

	let compare x y = ((Value.toInt x.value) - (Value.toInt y.value))

	let min x y = if ((Value.toInt x.value) <= (Value.toInt y.value)) then x else y
	let max x y = if ((Value.toInt x.value) >= (Value.toInt y.value)) then x else y

	let best lst = List.fold_left (fun acc x -> max acc x) (List.hd lst) lst



	let isOf card color =	(card.color = color)
	let isSpade card =		(card.color = Color.Spade)
	let isHeart card =		(card.color = Color.Heart)
	let isDiamond card =	(card.color = Color.Diamond)
	let isClub card =		(card.color = Color.Club)

end