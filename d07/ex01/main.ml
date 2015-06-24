
let main () =
	let doc = new Doctor.doctor "The Doctor (David Tennant)" in
	let p1 = new People.people "Rose Tyler" in
	p1#talk;
	doc#set_sidekick p1; doc#set_age 903; print_endline @@ doc#to_string;
	p1#die; doc#travel_in_time 2005 2008; doc#use_sonic_screwdriver;
	let p2 = new People.people "Donna Marble" in
	doc#set_sidekick p2; print_endline @@ doc#to_string;
	p2#talk; p1#die;

	doc#set_hp 60; print_endline @@ doc#to_string
	;doc#test_regenerate; print_endline @@ doc#to_string
	(* ;doc#regenerate; print_endline @@ doc#to_string *)

let () = ignore (main ())