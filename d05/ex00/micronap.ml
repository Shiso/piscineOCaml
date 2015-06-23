let my_sleep () = Unix.sleep 1


let main () =
	for x = 0 to ((int_of_string (Array.get Sys.argv 1)) - 1) do
		my_sleep ()
	done


let () = main ()