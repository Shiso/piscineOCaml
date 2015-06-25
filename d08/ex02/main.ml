let main () =
	let alkanes : Alkane.alkane list = [
		new Alkane.methane;
		new Alkane.ethane;
		new Alkane.octane;
	] in
	List.iter (fun (x:Alkane.alkane) -> print_endline (x#to_string)) alkanes

let () = ignore @@ main ()