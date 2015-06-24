include People

let main () =
	let p1 = new People.people "Rose Tyler" in
	p1#talk; p1#die;
	let p1 = new People.people "Clara Oswald" in
	p1#talk; p1#die

let () = ignore (main ())