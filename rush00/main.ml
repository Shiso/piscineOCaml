
type case = Nil
	| J1
	| J2
	| Board of case list



let toString = function
	| Nil -> "-"
	| J1  -> "O"
	| J2  -> "X"
	| _ -> "Yolo"


let getCase board x y =
	let bigN = (((x - 1) / 3) * 3) + ((y - 1) / 3) in
	let smallN = (((x - 1) mod 3) * 3) + ((y - 1) mod 3) in
	let rec loop n = function
		| [] -> Nil
		| h::t -> if n = 0 then h
				  else loop (n - 1) t
	in
	let tmp = loop bigN board in
	match tmp with
		| Board -> loop smallN (h::t)
		| _ -> tmp

let init_board () : case list =
	let board = [] in
	let rec fill_nil acc = function
		| 0 -> acc
		| n -> fill_nil (acc @ [Nil]) (n - 1)
	in
	let rec loop acc nb =
		if (nb = 0)
			then acc
		else
			loop (acc @ (fill_nil [] 9)) (nb - 1)
	in
	loop board 9

let print_board board =
	let rec loop c = function
		| 81 -> ()
		| nb -> 
			let c = getCase board (nb mod 9) (nb / 9) in
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