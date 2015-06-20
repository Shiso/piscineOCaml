include Card

let main () =
	let all = Card.all in
	let rec loop lst = match lst with
		| hd::tl -> print_string ((Card.toString hd) ^ "\t: ") ; print_endline (Card.toStringVerbose hd) ; loop tl
		| [] -> print_endline ""
	in
	loop all;

	let allSpade = Card.allSpades in
	let rec loopSpade lst = match lst with
		| hd::tl -> print_string ((Card.toString hd) ^ "\t: ") ; print_endline (Card.toStringVerbose hd) ; loopSpade tl
		| [] -> print_endline ""
	in
	loopSpade allSpade;

	let allHeart = Card.allHearts in
	let rec loopHeart lst = match lst with
		| hd::tl -> print_string ((Card.toString hd) ^ "\t: ") ; print_endline (Card.toStringVerbose hd) ; loopHeart tl
		| [] -> print_endline ""
	in
	loopHeart allHeart;

	let allDiamond = Card.allDiamonds in
	let rec loopDiamond lst = match lst with
		| hd::tl -> print_string ((Card.toString hd) ^ "\t: ") ; print_endline (Card.toStringVerbose hd) ; loopDiamond tl
		| [] -> print_endline ""
	in
	loopDiamond allDiamond;

	let allClub = Card.allClubs in
	let rec loopClub lst = match lst with
		| hd::tl -> print_string ((Card.toString hd) ^ "\t: ") ; print_endline (Card.toStringVerbose hd) ; loopClub tl
		| [] -> print_endline ""
	in
	loopClub allClub;
	let bestcard = Card.best all in
	print_string "Best of all : " ; print_endline (Card.toStringVerbose bestcard);
	print_endline "Best is : " ;
	print_string "\tSpade\t: " ; print_endline (string_of_bool ((Card.isSpade bestcard) && (Card.isOf bestcard Color.Spade)));
	print_string "\tHeart\t: " ; print_endline (string_of_bool (Card.isHeart bestcard));
	print_string "\tDiamond\t: " ; print_endline (string_of_bool (Card.isDiamond bestcard));
	print_string "\tClub\t: " ; print_endline (string_of_bool (Card.isClub bestcard));

	print_endline "Test compare : ";
	let c1 = Card.newCard Value.T7 Color.Diamond in
	let c2 = Card.newCard Value.Jack Color.Club in
	let c3 = Card.newCard Value.As Color.Spade in
	print_string "\t" ; print_string ((Card.toStringVerbose c1) ^ "\t" ^ (Card.toStringVerbose c2) ^ " : ") ; print_endline (string_of_int (Card.compare c1 c2));
	print_string "\t" ; print_string ((Card.toStringVerbose c3) ^ " \t" ^ (Card.toStringVerbose c2) ^ " : ") ; print_endline (string_of_int (Card.compare c3 c2));
	print_endline "Test min : ";
	print_string "\t" ; print_string ((Card.toStringVerbose c3) ^ " \t" ^ (Card.toStringVerbose c2) ^ " : ") ; print_endline (Card.toStringVerbose (Card.min c3 c2));
	print_endline "Test max : ";
	print_string "\t" ; print_string ((Card.toStringVerbose c3) ^ " \t" ^ (Card.toStringVerbose c2) ^ " : ") ; print_endline (Card.toStringVerbose (Card.max c3 c2))

let () = ignore (main ())