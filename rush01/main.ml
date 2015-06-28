(*
  ocamlfind ocamlc -package graphics -package camlimages -linkpkg test.ml -o test
 *)

(* charge une image quelconque (.jpg,.png... comme supporté par
   camlimages) vers une matrice de triplets (r,g,b) d'entiers :
   (int*int*int)*array*array *)
let load_rgb_matrix name =
  let img = Images.load name [] in
  let gimg = Graphic_image.array_of_image img in
  let rgb color =
    let quot n = n mod 256, n / 256 in
    let b, rg = quot color in
    let g, r = quot rg in
    r, g, b
  in
  Array.map (Array.map rgb) gimg

let n1 = load_rgb_matrix "assets/m1.png"
let n2 = load_rgb_matrix "assets/m2.png"
let n3 = load_rgb_matrix "assets/m3.png"
let u1 = load_rgb_matrix "assets/h1.png"
let u2 = load_rgb_matrix "assets/h2.png"
let u3 = load_rgb_matrix "assets/h3.png"
let u4 = load_rgb_matrix "assets/h4.png"
            (* sauvegarde une image grpahics de type Graphics.image dans le
   fichier au format de son choix (.png, .jpg...) *)
let save_image image file_name =
  let img = Images.Rgb24 (Graphic_image.image_of image) in
  Images.save file_name None [] img

              (* transforme une matrice de triplets (r,g,b) en une "image graphics"
   de type Graphics.image *)
let to_graphics rgb_matrix =
  Graphics.make_image
    (Array.map
       (Array.map
          (fun (r, g, b) -> Graphics.rgb r g b))
       rgb_matrix)

let map_matrix f matrix = Array.map (Array.map f) matrix

let invert_colors = map_matrix (fun (r, g, b) -> (255-r, 255-g, 255-b))

let ft_down x y w h =
  Graphics.set_color 0x000000;
  Graphics.fill_rect x y w h

let ft_put_image test obj =
  if test = n1 && obj#state then
    Graphics.draw_image (to_graphics u1) 360 149
  else if test = n2 && obj#state then
    Graphics.draw_image (to_graphics u2) 360 149
  else if test = n3 && obj#state then
    Graphics.draw_image (to_graphics u3) 360 149
  else
    Graphics.draw_image (to_graphics test) 360 149

let ft_draw obj =

  (* rectangle du haut *)
  Graphics.set_color 0xaa0010;
  Graphics.fill_rect 75 815 210 50;
  Graphics.fill_rect 355 815 210 50;
  Graphics.fill_rect 635 815 210 50;
  Graphics.fill_rect 915 815 210 50;
  (* rectangle du bas *)
  Graphics.fill_rect 75 50 210 50; 
  Graphics.fill_rect 355 50 210 50;
  Graphics.fill_rect 635 50 210 50;
  Graphics.fill_rect 915 50 210 50;
  Graphics.fill_rect 915 120 210 50;
  (* menu du cote *)
  Graphics.fill_rect 1020 250 100 50;
  Graphics.fill_rect 1020 350 100 50;
  Graphics.fill_rect 1020 450 100 50;


  Graphics.set_color 0xFF0032;
  Graphics.fill_rect 80 820 (obj#get_health * 2) 40;
  Graphics.fill_rect 360 820 (obj#get_energy * 2) 40;
  Graphics.fill_rect 640 820 (obj#get_hygiene * 2) 40;
  Graphics.fill_rect 920 820 (obj#get_happyness * 2) 40;

  (* rectangle du bas *)
  Graphics.fill_rect 78 53 204 44; 
  Graphics.fill_rect 358 53 204 44;
  Graphics.fill_rect 638 53 204 44;
  Graphics.fill_rect 918 53 204 44;
  Graphics.fill_rect 918 123 204 44;
  (* menu du cote *)
  Graphics.fill_rect 1023 253 94 44;
  Graphics.fill_rect 1023 353 94 44;
  Graphics.fill_rect 1023 453 94 44;

  Graphics.set_color 0xCCCCCC;
  Graphics.moveto 150 75;
  Graphics.draw_string ("MAKE HIM EAT");
  Graphics.moveto 430 75;
  Graphics.draw_string ("THUNDER STRUCK");
  Graphics.moveto 680 75;
  Graphics.draw_string ("TAKE A BATH");
  Graphics.moveto 960 75;
  Graphics.draw_string ("KILL SOMETHING");
  Graphics.moveto 960 145;
  Graphics.draw_string ("HIT COKESHROOM");

  Graphics.moveto 1060 468;
  Graphics.draw_string ("QUIT");

  Graphics.moveto 1060 368;
  Graphics.draw_string ("RESET");

  Graphics.moveto 1060 268;
  Graphics.draw_string ("SAVE");

  Graphics.moveto 150 830;
  Graphics.draw_string ("HEALTH : " ^ (string_of_int obj#get_health));
  Graphics.moveto 430 830;
  Graphics.draw_string ("ENERGY : " ^ (string_of_int obj#get_energy));
  Graphics.moveto 680 830;
  Graphics.draw_string ("HYGIENE : " ^ (string_of_int obj#get_hygiene));
  Graphics.moveto 960 830;
  Graphics.draw_string ("HAPPYNESS : " ^ (string_of_int obj#get_happyness))


let getTick obj tick =
	let newTick = Sys.time () in
	if newTick -. tick >= 1. then begin
    ft_draw obj;
		(obj#tic;newTick)
  end
  else
    tick

let rec loop test (obj:Pokemon.obj) bouboul tick index =
  let info = Graphics.wait_next_event [Graphics.Poll] in
  let bol = info.Graphics.button in
  let newTick =  getTick obj tick in
  if (index = 0) then begin 
   ft_put_image n1 obj;ft_draw obj
  end
  else if (index = 10000) then begin
    ft_put_image n2 obj; ft_draw obj
  end
  else if (index = 20000) then begin
    ft_put_image n3 obj; ft_draw obj
  end
  else if (index = 30000) then begin
    ft_put_image n2 obj; ft_draw obj
  end;
    if (bol = false && bouboul = true && info.Graphics.mouse_x >= 75 && info.Graphics.mouse_x <= 285 && info.Graphics.mouse_y >= 50 && info.Graphics.mouse_y <= 100) then begin
      (obj#eat);ft_draw obj;loop test obj bol newTick (index+1)
  	end else if (bol = false && bouboul = true && info.Graphics.mouse_x >= 355 && info.Graphics.mouse_x <= 565 && info.Graphics.mouse_y >= 50 && info.Graphics.mouse_y <= 100) then begin
  		(obj#thunder);ft_draw obj;loop test obj bol newTick (index+1)
  	end else if (bol = false && bouboul = true && info.Graphics.mouse_x >= 635 && info.Graphics.mouse_x <= 845 && info.Graphics.mouse_y >= 50 && info.Graphics.mouse_y <= 100) then begin
  		(obj#bath);ft_draw obj;loop test obj bol newTick (index+1)
  	end else if (bol = false && bouboul = true && info.Graphics.mouse_x >= 915 && info.Graphics.mouse_x <= 1125 && info.Graphics.mouse_y >= 50 && info.Graphics.mouse_y <= 100) then begin
  		(obj#kill);ft_draw obj;loop test obj bol newTick (index+1)
  	end else if (bol = false && bouboul = true && info.Graphics.mouse_x >= 1020 && info.Graphics.mouse_x <= 1120 && info.Graphics.mouse_y >= 250 && info.Graphics.mouse_y <= 300) then begin
  		(obj#save);ft_draw obj;loop test obj bol newTick (index+1)
    end else if (bol = false && bouboul = true && info.Graphics.mouse_x >= 1020 && info.Graphics.mouse_x <= 1120 && info.Graphics.mouse_y >= 450 && info.Graphics.mouse_y <= 500) then 
      raise ( Graphics.Graphic_failure "e")
    else if (bol = true && bouboul = false && info.Graphics.mouse_x >= 915 && info.Graphics.mouse_x <= 1125 && info.Graphics.mouse_y >= 120 && info.Graphics.mouse_y <= 170) then begin
      (obj#hit); ft_put_image u4 obj; ft_draw obj; loop test obj bol newTick (index+1)
    end else if (bol = false && bouboul = true && info.Graphics.mouse_x >= 1020 && info.Graphics.mouse_x <= 1120 && info.Graphics.mouse_y >= 350 && info.Graphics.mouse_y <= 400) then begin
      obj#reset;ft_draw obj;loop test obj bol newTick (index+1)   
    end
      

    else if (bol = true && bouboul = false && info.Graphics.mouse_x >= 75 && info.Graphics.mouse_x <= 285 && info.Graphics.mouse_y >= 50 && info.Graphics.mouse_y <= 100) then begin 
      ft_down 75 50 210 50;loop test obj bol newTick (index+1)
    end else if (bol = true && bouboul = false && info.Graphics.mouse_x >= 355 && info.Graphics.mouse_x <= 565 && info.Graphics.mouse_y >= 50 && info.Graphics.mouse_y <= 100) then begin 
      ft_down 355 50 210 50;loop test obj bol newTick (index+1)
    end else if (bol = true && bouboul = false && info.Graphics.mouse_x >= 635 && info.Graphics.mouse_x <= 845 && info.Graphics.mouse_y >= 50 && info.Graphics.mouse_y <= 100) then begin 
      ft_down 635 50 210 50;loop test obj bol newTick (index+1)
    end else if (bol = true && bouboul = false && info.Graphics.mouse_x >= 915 && info.Graphics.mouse_x <= 1125 && info.Graphics.mouse_y >= 50 && info.Graphics.mouse_y <= 100) then begin 
      ft_down 915 50 210 50;loop test obj bol newTick (index+1)
    end else if (bol = true && bouboul = false && info.Graphics.mouse_x >= 1020 && info.Graphics.mouse_x <= 1120 && info.Graphics.mouse_y >= 250 && info.Graphics.mouse_y <= 300) then begin 
      ft_down 1020 250 100 50;loop test obj bol newTick (index+1)
    end else if (bol = true && bouboul = false && info.Graphics.mouse_x >= 1020 && info.Graphics.mouse_x <= 1120 && info.Graphics.mouse_y >= 450 && info.Graphics.mouse_y <= 500) then begin 
      ft_down 1020 450 100 50;loop test obj bol newTick (index+1)
     end else if (bol = true && bouboul = false && info.Graphics.mouse_x >= 1020 && info.Graphics.mouse_x <= 1120 && info.Graphics.mouse_y >= 350 && info.Graphics.mouse_y <= 400) then begin
      ft_down 1020 350 100 50;loop test obj bol newTick (index+1)
     end else if (bol = true && bouboul = false && info.Graphics.mouse_x >= 915 && info.Graphics.mouse_x <= 1125 && info.Graphics.mouse_y >= 120 && info.Graphics.mouse_y <= 170) then begin
      ft_down 915 120 210 50;loop test obj bol newTick (index+1)
    end else begin
      if index > 40000 then
      	loop test obj bol newTick 0
      else
        loop test obj bol newTick (index+1)
    end

let get_save () =
	let o = new Pokemon.obj in
	try let o1 = o#load in o1 with _ -> o

let main () =
	let obj = get_save () in

	Graphics.open_graph " 1200x900";

  ft_draw obj;
	try loop n1 obj false (Sys.time ()) 0 with  Graphics.Graphic_failure e -> obj#save;print_endline "game saved"
                                              | _ -> obj#reset;obj#save;print_endline "Game Over";
	Graphics.close_graph ()

let () = main ()
