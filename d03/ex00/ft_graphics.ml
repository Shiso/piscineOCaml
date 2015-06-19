type 'a tree = Nil | Node of 'a * 'a tree * 'a tree

let draw_square ?(text="") x y =
	let margx = 42 in
	let margy = 21 in
	Graphics.moveto (x - margx + 10) (y - 7);
	Graphics.draw_string text;
	Graphics.moveto (x - margx) (y - margy);
	Graphics.lineto (x + margx) (y - margy);
	Graphics.lineto (x + margx) (y + margy);
	Graphics.lineto (x - margx) (y + margy);
	Graphics.lineto (x - margx) (y - margy)

let draw_tree_node node_item =
	let loop node = match node with
		| Nil -> draw_square ~text:"Nil" 400 400
		| Node (value, Nil, Nil) ->
			draw_square ~text:value 50 400;
			Graphics.moveto (50 + 42) 400;
			Graphics.lineto (150 - 42) 350;
			Graphics.moveto (50 + 42) 400;
			Graphics.lineto (150 - 42) 450;
			draw_square ~text:"Nil" 150 350;
			draw_square ~text:"Nil" 150 450
		| _ -> ()
	in
	loop node_item


let main () =
	Graphics.open_graph " 800x600";
	draw_square ~text:"Box 1" 50 30;
	draw_square ~text:"Box 2" 150 30;
	draw_square 250 30;
	let l1 = Nil in
	let l2 = Nil in
	let node = Node ("Yolo", l1, l2) in
	draw_tree_node node;
	Graphics.read_key ()


let () =
	ignore (main ());
	print_endline ""