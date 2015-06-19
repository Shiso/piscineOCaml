
let is_lower c = ((c >= 'a') && (c <= 'z'))
let is_upper c = ((c >= 'A') && (c <= 'Z'))

let rot ?(nb=42) (c:char) = 
	if ((is_upper c) || (is_lower c)) then
		let rot = (nb mod 26) in
		if (((int_of_char c) + rot) > 122 && (is_lower c)) then
			(char_of_int (97 + (((int_of_char c) + rot) - 122)))
		else if (((int_of_char c) + rot) > 90 && (is_upper c)) then
			(char_of_int (65 + (((int_of_char c) + rot) - 90)))
		else (char_of_int ((int_of_char c) + rot))
	else c


let cxor ~key c =
  char_of_int ((int_of_char c) lxor key)

let root42 text =
	String.map rot text

let caesar text nb =
	String.map (rot ~nb:nb) text

let xor text key =
	String.map (cxor ~key:key) text