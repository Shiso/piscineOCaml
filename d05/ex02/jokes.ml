
let main () =
	Random.self_init ();
	let arr = Array.make 5 "" in
	Array.set arr 0 "C'est l'histoire d'un pingouin qui respirait par le cul, un jour il s'assoie et il meure.";
	Array.set arr 1 "c'est toto qui est dans l'espace, il pete dans sa combinaison et il explose.";
	Array.set arr 2 "Cette histoire pas drole ne l'est definitivement pas.";
	Array.set arr 3 "Si j'eût été chez ta soeur, ç'aurait été a dessein";
	Array.set arr 4 "C'est l'histoire d'un mec qui rentre dans un bar, et le barman lui dit : \"C'est pour une blague ?\"";
	print_endline (arr.((Random.int 5)))

let () = ignore (main ())