
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
	let bigN = (((x - 1) / 3) * 3) + ((y - 1) / 3) in
	let smallN = (((x - 1) mod 3) * 3) + ((y - 1) mod 3) in
	let rec loop n = function
		| [] -> {chip=Nil; inc=[]}
		| h::t -> if n = 0 then h
				  else loop (n - 1) t
	in
	let tmp = loop bigN board.inc in
	let {chip=n; inc=l} = tmp in
	if l = [] then n
	else let ret = loop smallN l in ret.chip

let init_board () : case =
	let board = {chip=Nil; inc=[]} in
	let rec fill_nil acc = function
		| 0 -> acc
		| n when n = 3 -> fill_nil (acc @ [{chip=P1; inc=[]}]) (n - 1)
		| n when n = 5 -> fill_nil (acc @ [{chip=P2; inc=[]}]) (n - 1)
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

let main () =
	let board = init_board () in
	print_board board
let () = main ()