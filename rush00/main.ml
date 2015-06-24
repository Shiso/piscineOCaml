
type value = Nil
	| P1
	| P2

type case = {
	chip: value;
	inc:  case list
}


let toString = function
	| Nil -> "-"
	| P1  -> "O"
	| P2  -> "X"


let getCase board x y =
	let bigN = ((x / 3) * 3) + (y / 3) in
	let smallN = ((x mod 3) * 3) + (y mod 3) in
	let {chip=n; inc=l} = board in
	if l = [] then n
	else
	let tmp = List.nth board.inc bigN in
	let {chip=n; inc=l} = tmp in
	if l = [] then n
	else let ret = List.nth l smallN in ret.chip

let getSmallCase board x y =
	let nb = (x * 3) + y in
	let {chip=n; inc=l} = board in
	if l = [] then n
	else
	let tmp = List.nth board.inc nb in
	let {chip=n; inc=l} = tmp in n

let replace_case n cell board =
	let rec loop n new_cell = function
		| [] -> []
		| h::t when n = 0 -> new_cell::t
		| h::t -> h :: (loop (n - 1) new_cell t)
	in
	{chip=Nil; inc=loop n cell board.inc}

let play board x y player =
	let bigN = ((x / 3) * 3) + (y / 3) in
	let smallN = ((x mod 3) * 3) + (y mod 3) in
	let tmp = List.nth board.inc bigN in
	let small = replace_case smallN {chip=player; inc=[]} tmp in
	replace_case bigN small board

let init_board () : case =
	let board = {chip=Nil; inc=[]} in
	let rec fill_nil acc = function
		| 0 -> acc
		| n -> fill_nil (acc @ [{chip=Nil; inc=[]}]) (n - 1)
	in
	let rec loop acc nb =
		if (nb = 0)
			then acc
		else
			loop (acc @ [{chip=Nil; inc=(fill_nil [] 9)}]) (nb - 1)
	in
	let ret = {chip=Nil; inc=(loop board.inc 9)} in ret

let print_board board =
	let rec loop c = function
		| 81 -> ()
		| nb -> 
			let c = toString @@ getCase board (nb mod 9) (nb / 9) in
			if (((nb + 1) mod 9) = 0) then print_string (c^"\n")
			else if (((nb + 1) mod 3) = 0) then print_string (c^" | ")
			else print_string (c^" ") ;
			if (((nb + 1) mod 27) = 0 && nb < 80) then print_string "---------------------\n";
			loop board (nb + 1)
	in
	loop board 0

let check_input board s =
	let is_valid c =
		let int_c = int_of_char c in
			int_c >= 49 && int_c <= 57
	in
	if (String.length s) = 3
		&& is_valid @@ String.get s 0
		&& is_valid @@ String.get s 2
		&& String.get s 1 = ' '
	then let x = ((int_of_char @@ String.get s 0) - 49) in
		 let y = ((int_of_char @@ String.get s 2) - 49) in
		 let chip = (getCase board x y) in
		if chip != Nil then ((-2), (-2)) else (x, y)
 	else ((-1), (-1))

let check_board player board : value =
	let rec check_draw = function
		| [] -> player
		| h::t -> if (h.chip = Nil) then Nil else check_draw t
	in
	let draw = check_draw board.inc in
	if draw <> Nil then begin draw end
	else
	let check_triplet l =
		match l with
		 | (p1, p2, p3) when p1 = p2 && p2 = p3 && p1 <> Nil -> p1
		 | _ ->	Nil
	in
	let rec hor_check x = match x with
		| 3 ->	Nil
		| _ ->	let chip = check_triplet ((getSmallCase board x 0), (getSmallCase board x 1), (getSmallCase board x 2)) in
				if chip <> Nil then chip else hor_check (x + 1)
	in
	let rec ver_check x = match x with
		| 3 ->	Nil
		| _ ->	let chip = check_triplet ((getSmallCase board 0 x), (getSmallCase board 1 x), (getSmallCase board 2 x)) in
				if chip <> Nil then chip else ver_check (x + 1)
	in
	let hor = (hor_check 0) in
	let ver = (ver_check 0) in
	let dig1 = check_triplet ((getSmallCase board 0 0), (getSmallCase board 1 1), (getSmallCase board 2 2)) in
	let dig2 = check_triplet ((getSmallCase board 2 0), (getSmallCase board 1 1), (getSmallCase board 0 2)) in
	if hor <> Nil then hor
	else if ver <> Nil then ver
	else if dig1 <> Nil then dig1
	else if dig2 <> Nil then dig2
	else Nil

let check_board_win player board =
	let rec loop = function
	| 9 -> board
	| n -> 
		let tic = List.nth board.inc n in
		
		let chip = check_board player tic in
		if chip <> Nil && tic.inc <> [] then begin
			print_string @@ (toString chip)^" wins grid "^(string_of_int (n + 1))^"!\n";
			replace_case n {chip=chip; inc=[]} board
		end
		else loop (n + 1)
	in
	loop 0

let main () =
	let board = init_board () in
	print_board board; print_newline ();
	let rec main_loop board player =
		print_endline @@ (toString player)^"'s turn to play.";
		let input = check_input board (read_line ()) in
		match input with
			| (-2, -2) -> print_endline "Illegal move."; main_loop board player;
			| (-1, -1) -> print_endline "Incorrect format."; main_loop board player;
			| (x, y) -> let board = play board x y player in
		let board = check_board_win player board in
		let winner = check_board player board in
		if winner = Nil then begin
		print_newline (); print_board board; print_newline ();
			if (player = P2) then main_loop board P1 else main_loop board P2; end
		else begin print_string @@ (toString winner)^" wins the game!\n"; board end
	in
	let board = main_loop board P1 in
	print_newline (); print_board board; print_newline ()

let () = main ()