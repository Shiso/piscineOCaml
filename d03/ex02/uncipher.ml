
let is_lower c = ((c >= 'a') && (c <= 'z'))
let is_upper c = ((c >= 'A') && (c <= 'Z'))

let unrot ?(nb=42) (c:char) = 
	if ((is_upper c) || (is_lower c)) then
		let rot = (nb mod 26) in
		if (((int_of_char c) - rot) < 97 && (is_lower c)) then
			(char_of_int (122 - (97 - ((int_of_char c) - rot))))
		else if (((int_of_char c) - rot) < 65 && (is_upper c)) then
			(char_of_int (90 - (65 - ((int_of_char c) - rot))))
		else (char_of_int ((int_of_char c) - rot))
	else c


let unroot42 text =
	String.map unrot text

let uncaesar text nb =
	String.map (unrot ~nb:nb) text

let main () =
	print_endline "Root 42";
	print_endline (Cipher.root42 "abcdxyzABCD VWXYZ");
	print_endline (unroot42 (Cipher.root42 "abcdxyzABCD VWXYZ"));
	print_endline "Caesar by 1";
	print_endline (Cipher.caesar "abcdxyzABCD VWXYZ" 1);
	print_endline (uncaesar (Cipher.caesar "abcdxyzABCD VWXYZ" 1) 1);
	print_endline "Xor";
	print_endline (Cipher.xor "abcdxyzABCD VWXYZ" 1);
	print_endline (Cipher.xor (Cipher.xor "abcdxyzABCD VWXYZ" 1) 1)
	

let () = main ()