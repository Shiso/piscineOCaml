type 'a tree = Nil | Node of 'a * 'a tree * 'a tree

let draw_square ?(text="") x y =
	let (tw, th) = Graphics.text_size text in
	let margx = (tw + 20) / 2 in
	let margy = (th + 8) / 2 in
	Graphics.moveto (x - (tw / 2)) (y - (th / 2));
	Graphics.draw_string text;
	Graphics.moveto (x - margx) (y - margy);
	Graphics.lineto (x + margx) (y - margy);
	Graphics.lineto (x + margx) (y + margy);
	Graphics.lineto (x - margx) (y + margy);
	Graphics.lineto (x - margx) (y - margy)

let rec size node = match node with
		| Node (value, nodeleft, noderight) -> 1 + (size nodeleft) + (size noderight)
		| _ -> 0

let rec height node = match node with
		| Node (value, nodeleft, noderight) ->
			if (size nodeleft) > (size noderight) then
				(1 + (height nodeleft))
			else begin
				(1 + (height noderight))
			end
		| _ -> -1

let get_pos y ymax x xmax o =
	let width = Graphics.size_x () in
	let height = Graphics.size_y () in
	let posx = (((width - 120) / (xmax + 2)) + 10) * x + 60 in
	let posy = ((height / 2) / ymax) * y + o in
	(posx, posy)


let draw_tree (root:string tree) =
	let cols = (height root) in
	let o = (Graphics.size_y ()) / 2 in
	let rec loop ?(i=0) ?(depth=0) node o =
		let maxY = (int_of_float ((2. ** (float_of_int depth)))) in
		let (x, y) = get_pos i maxY depth cols o in
		match node with
			| Node (value, nodeleft, noderight) ->
				let (margx, margy) = Graphics.text_size value in
				let margx = (margx + 20) / 2 in
				if (depth > 0) then Graphics.lineto (x - margx) y;
				draw_square ~text:value x y;
				Graphics.moveto (x + margx) y;
				loop ~i:(1) ~depth:(depth + 1) nodeleft y;
				Graphics.moveto (x + margx) y;
				loop ~i:(-1) ~depth:(depth + 1) noderight y;
			| _ ->
				let (margx, margy) = Graphics.text_size "Nil" in
				let margx = (margx + 20) / 2 in
				if (depth > 0) then Graphics.lineto (x - margx) y;
				draw_square ~text:"Nil" x y
	in
	loop root o


let main () =
	Graphics.open_graph " 800x600";
	draw_square ~text:"Box 1" 50 30;
	draw_square ~text:"Box 2" 150 30;
	draw_square 250 30;

	let last = Node ("last", Nil, Nil) in

	let n1 = Node ("n 1", last, Nil) in
	let n2 = Node ("n 2", Nil, Nil) in
	let n3 = Node ("n 3", Nil, Nil) in

	let m2 = Node ("m 2", n1, Nil) in
	let m1 = Node ("m 1", n2, n3) in

	let first = Node ("first", m1, m2) in

	print_string "tree size : "; print_int (size (first)); print_endline "";
	print_string "tree height : "; print_int (height (first)); print_endline "";

	draw_tree first;

	Graphics.read_key ()


let () =
	ignore (main ());
	print_endline ""