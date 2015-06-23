
let eu_dist a b : float =
	let rec loop a b n =
		let size = (Array.length a) - 1 in
		if size <> ((Array.length b) - 1) then failwith "Arrays non compatibles (tailles differentes)"
		else if n <= size then (((a.(n) -. b.(n)) ** 2.) +. (loop a b (n + 1)))
		else 0.
	in
	sqrt (loop a b 0)


let test a b =
	Printf.printf "Test eu_dist avec: \n%!";
	Printf.printf "a: [| %!";
	Array.iter ((fun value -> print_string (string_of_float value); if 1 <> (Array.length a) then print_string "; ")) a;
	Printf.printf " |]\nb: [| %!";
	Array.iter ((fun value -> print_string (string_of_float value); if 1 <> (Array.length b) then print_string "; ")) b;
	Printf.printf " |]\n";
	Printf.printf "eu_dist a b = %!";
	try
		Printf.printf "%.f\n" (eu_dist a b)
	with
		| Failure m	->	Printf.printf "Error \"%s\"\n" m

let main () =
	let aa = Array.init 4 (fun x -> float_of_int (x)) in
	let bb = Array.init 4 (fun x -> float_of_int (x * x)) in
	test aa bb;
	test [|42.|] [|-42.|];
	test [|-42.|] [|42.|];
	test [|21.|] [|5.|];
	test [|24.; 65.; 78.|] [|897.; 234.;8.|];
	test [|24.; 65.; 78.|] [|897.; 234.|]

let () = ignore (main ())