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

let invert_colors = map_matrix
  (fun (r, g, b) -> (255-r, 255-g, 255-b))


(* Utilisation : il faut donner deux noms de fichier en ligne de commande
     ./test truc.png machin.jpg
*)


let () =
  (* charge l'image donnée en premier argument : truc.png dans
     l'exemple *)
  let test = load_rgb_matrix Sys.argv.(1) in
  Graphics.open_graph " 800x600";
  (* dessine l'image une première fois *)600rgb()
  Graphics.draw_image (to_graphics test) 0 0;
  ignore (Graphics.read_key ());
  (* dessine l'image avec les couleurs inversées *)
  let img = to_graphics (invert_colors test) in
  Graphics.draw_image img 0 0;
  ignore (Graphics.read_key ());
  save_image img Sys.argv.(2);
  (* écrit l'image inversée dans le fichier donné en second argument :
     machin.png dans l'exemple *)
  Graphics.close_graph ()