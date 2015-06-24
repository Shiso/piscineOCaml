
let main () =
	let doc = new Doctor.doctor "The Doctor (David Tennant)" in
	let p1 = new People.people "Rose Tyler" in
	let p2 = new People.people "Donna Marble" in
	let docdo = new Doctor.doctor "Doctor Donna" in
	p1#talk;
	doc#set_sidekick p1; doc#set_age 903; print_endline doc#to_string;

	doc#travel_in_time 2005 2008;

	let dalek1 = new Dalek.dalek in
	let dalek2 = new Dalek.dalek in
	let dalek3 = new Dalek.dalek in

	let dalekEmpire = new Army.army in
	let humans = new Army.army in
	let doctors = new Army.army in

	dalek1#talk;
	dalek2#talk;
	dalek3#talk;

	dalekEmpire#add dalek1;
	dalekEmpire#add dalek2;
	dalekEmpire#add dalek3;

	print_endline "3 Darlek army ";
	List.iter (fun item -> print_endline (item#get_name)) dalekEmpire#get;
	dalekEmpire#remove;
	print_endline "2 Darlek army ";
	List.iter (fun item -> print_endline (item#get_name)) dalekEmpire#get;
	dalekEmpire#add dalek1;
	print_endline "3 Darlek army again";
	List.iter (fun item -> print_endline (item#get_name)) dalekEmpire#get;

	humans#add p1;
	humans#add p2;
	print_endline "2 Humans army ";
	List.iter (fun item -> print_endline (item#get_name)) humans#get;

	doctors#add doc;
	doctors#add docdo;
	print_endline "2 Doctors army ";
	List.iter (fun item -> print_endline (item#get_name)) doctors#get

let () = ignore (main ())