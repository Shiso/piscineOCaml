
let main () =
	Random.self_init ();
	let file = open_in "bad.jokes" in
	let line = input_line file in
	let size = int_of_string line in
	let jokes = Array.make size "" in
	for i = 0 to (size - 1) do
		let line = input_line file in
		jokes.(i) <- line
	done;
	print_endline (jokes.((Random.int size)))

let () = ignore (main ())