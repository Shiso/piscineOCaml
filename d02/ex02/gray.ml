let gray len =
	let rec loop acc =
		(* print acc *)
		let tmp_nb = acc lsr 1 in
		let ret = acc lxor tmp_nb in
		loop ret
		(* output_binary_int stdout ret *)
	in
	loop 5


let main () =
	gray (0)(* ;
	gray (1);
	gray (2);
	gray (3)
 *)

let () = main ()