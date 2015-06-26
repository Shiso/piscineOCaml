include Watchtower

let main () =
	let a:Watchtower.hour = Watchtower.zero
	and b:Watchtower.hour = 6
	and c:Watchtower.hour = 15
	and d:Watchtower.hour = 8
	and e:Watchtower.hour = (-6)
	and f:Watchtower.hour = (-42) in
	print_endline "Add tests :";
	Watchtower.print_hour a; Watchtower.print_hour b; Watchtower.print_hour (Watchtower.add a b); print_endline "";
	Watchtower.print_hour c; Watchtower.print_hour d; Watchtower.print_hour (Watchtower.add c d); print_endline "";
	Watchtower.print_hour e; Watchtower.print_hour b; Watchtower.print_hour (Watchtower.add e b); print_endline "";
	Watchtower.print_hour f; Watchtower.print_hour e; Watchtower.print_hour (Watchtower.add f e); print_endline "";

	print_endline "Sub tests :";
	Watchtower.print_hour a; Watchtower.print_hour b; Watchtower.print_hour (Watchtower.sub a b); print_endline "";
	Watchtower.print_hour c; Watchtower.print_hour d; Watchtower.print_hour (Watchtower.sub c d); print_endline "";
	Watchtower.print_hour e; Watchtower.print_hour b; Watchtower.print_hour (Watchtower.sub e b); print_endline "";
	Watchtower.print_hour f; Watchtower.print_hour e; Watchtower.print_hour (Watchtower.sub f e); print_endline "";
	Watchtower.print_hour f; Watchtower.print_hour c; Watchtower.print_hour (Watchtower.sub f c); print_endline ""


let () = main ()