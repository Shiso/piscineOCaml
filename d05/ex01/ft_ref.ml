type 'a my_ref = {mutable value : 'a}

let return a : 'a my_ref = {value = a}

let get (item : 'a my_ref) : 'a = item.value

let set (item : 'a my_ref) value = item.value <- value

let bind ({value}: 'a my_ref) (f:'a -> 'b my_ref) = f value

let main () =
	let mref = return 8 in
	print_int (get mref) ; print_endline " get ( : int my_ref)";
	set mref 42 ; print_endline " set";
	print_int (get mref) ; print_endline " get ( : int my_ref)";
	print_endline "bind : (fun a -> return (string_of_int a)) ";
	let newref = bind mref (fun a -> return (string_of_int a)) in
	print_endline ((get newref)^" ( : string my_ref)")

let () = ignore (main ())