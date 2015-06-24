
let main () =
	let doc = new Doctor.doctor "The Doctor (David Tennant)" in
	let p1 = new People.people "Rose Tyler" in
	p1#talk;
	doc#set_sidekick p1; doc#set_age 903; print_endline @@ doc#to_string;

	doc#travel_in_time 2005 2008;

	let dalek = new Dalek.dalek in
	dalek#talk;
	p1#talk;
	doc#talk;
	dalek#talk;
	dalek#talk;
	print_endline "the doctor aim at the target with his sonic screwdriver";
	doc#use_sonic_screwdriver;
	print_string "the darlek aim at " ; print_endline (p1#get_name);
	dalek#exterminate p1;
	print_endline @@ dalek#to_string;
	doc#use_sonic_screwdriver;
	dalek#die

let () = ignore (main ())