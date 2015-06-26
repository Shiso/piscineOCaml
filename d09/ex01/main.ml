include App

let print_proj (x:App.project) = match x with | (name, status, grad) -> print_endline (name^" "^status^" "^(string_of_int grad))

let main () =
	let a = App.zero
	and b = ("La puissance de la funk ", "fail", 60)
	and c = ("yolo ", "", 0)
	and d = ("What is it ?", "Success", 120) in
	print_endline "Simple print :";
	print_proj a;
	print_proj b;
	print_proj c;
	print_proj d;


	print_endline "\n\nCombine :";
	print_proj a; print_proj b; print_proj (App.combine a b);
	print_endline "------------";
	print_proj c; print_proj d; print_proj (App.combine c d);
	print_endline "------------";
	print_proj b; print_proj d; print_proj (App.combine b d);


	print_endline "\n\nFail :";
	print_proj b; print_proj (App.fail b);
	print_endline "------------";
	print_proj c; print_proj (App.fail c);
	print_endline "------------";
	print_proj d; print_proj (App.fail d);


	print_endline "\n\nSuccess :";
	print_proj b; print_proj (App.success b);
	print_endline "------------";
	print_proj c; print_proj (App.success c);
	print_endline "------------";
	print_proj d; print_proj (App.success d);


	print_endline "\n\nMega test b+d+(success c):";
	print_proj b; print_proj d; print_proj c;
	print_endline "\n\nsuccess C:";
	print_proj (App.success c);
	print_endline "\n\nres:";
	print_proj (App.combine (App.combine b d) (App.success c))


let () = main ()