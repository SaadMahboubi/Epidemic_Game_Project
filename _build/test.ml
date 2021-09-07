(* open Graphics;;

let width_size  = 1500;;
let height_size = 1000;;
let rect_size   = 50;;

let grille () =
    let nb_rect_by_line = width_size / rect_size in
    set_color black;

    for i = 0 to nb_rect_by_line do
        for j = 0 to nb_rect_by_line do
            set_color black;
            draw_rect (rect_size * i) (rect_size * j) rect_size rect_size
        done
    done
;;

open_graph (Printf.sprintf " %ix%i" width_size height_size);; (* espace obligatoire avant le %i *)

grille ();;

ignore (wait_next_event [ Key_pressed ]);;
*)

open Graphics;;


let afficher_grille grille =

  (* Reset de l'affichage *)
  Graphics.open_graph("");
  Graphics.clear_graph();


  (* Calcul des dimensions *)
  let largeur_fenetre = Graphics.size_x()
  and hauteur_fenetre = Graphics.size_y() in

  let largeur_grille = Array.length grille.(0)
  and hauteur_grille = Array.length grille in

  let marge = 10 in

  let largeur_dispo = largeur_fenetre - (marge * 2)
  and hauteur_dispo = hauteur_fenetre - (marge * 2) in

  let largeur_max = largeur_dispo / largeur_grille
  and hauteur_max = hauteur_dispo / hauteur_grille in
  let taille_case = min largeur_max hauteur_max in

  let x0 = marge + (largeur_dispo - taille_case * largeur_grille) / 2
  and y0 = marge + (hauteur_dispo - taille_case * hauteur_grille) / 2 in
  let yM = y0 + hauteur_grille*taille_case in


  (* Trace de la grille *)
  Graphics.set_color (Graphics.rgb 192 192 192);

  for i = 0 to largeur_grille
  do
    Graphics.moveto (x0 + i*taille_case) y0;
    Graphics.rlineto 0 (hauteur_grille*taille_case)
  done;

  for j = 0 to hauteur_grille
  do
    Graphics.moveto x0 (y0 + j*taille_case);
    Graphics.rlineto (largeur_grille*taille_case) 0
  done;


  (* Trace du contenu de la grille *)
  Graphics.set_color Graphics.black;

  for ligne = 0 to hauteur_grille - 1
  do
    for colonne = 0 to largeur_grille - 1
    do
      if (grille.(ligne).(colonne))
      then
	begin
	  let x = x0 + 1 + colonne*taille_case
	  and y = yM - (taille_case) - ligne*taille_case in
	  Graphics.fill_rect x y (taille_case-1) (taille_case-1)
	end
    done
  done;;



(* ----- Fonction d'attente passive ----- *)
open Unix;;
(*#load "unix.cma";;*)

let attendre ms =
  let start = Unix.gettimeofday() in
  let s = (float_of_int ms) *. 0.001 in
  let rec delay t =
    try
      ignore (Unix.select [] [] [] t)
    with Unix.Unix_error(Unix.EINTR, _, _) ->
      let now = Unix.gettimeofday() in
      let remaining = start +. s -. now in
      if remaining > 0.0
      then delay remaining
  in delay s;;



(* ----- Exemples ----- *)

let g_test_1 = [|
[| false; false; false; false; false |];
[| false; false; false; false; false |];
[| false; true; true; true; false |];
[| false; false; true; false; false |];
[| false; false; false; false; false |];
[| false; false; false; false; false |] |];;


let g_test_2 = [|
   [|false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false|] ;
   [|false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false|] ;
   [|false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false|] ;
   [|false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false|] ;
   [|false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false|] ;
   [|false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false|] ;
   [|false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false|] ;
   [|false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false|] ;
   [|false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false|] ;
   [|false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false|] ;
   [|false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;true;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false|] ;
   [|false;false;false;false;false;false;false;false;false;false;false;false;false;false;true;false;true;false;false;false;false;false;false;false;false;false;false;false;false;false;false|] ;
   [|false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;true;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false|] ;
   [|false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false|] ;
   [|false;false;false;false;false;false;false;false;false;false;false;false;false;true;true;true;true;true;false;false;false;false;false;false;false;false;false;false;false;false;false|] ;
   [|false;false;false;false;false;false;false;false;false;false;false;false;true;false;false;false;false;false;true;false;false;false;false;false;false;false;false;false;false;false;false|] ;
   [|false;false;false;false;false;false;false;false;false;false;false;true;false;true;true;false;true;true;false;true;false;false;false;false;false;false;false;false;false;false;false|] ;
   [|false;false;false;false;false;false;false;false;false;false;false;false;true;false;false;false;false;false;true;false;false;false;false;false;false;false;false;false;false;false;false|] ;
   [|false;false;false;false;false;false;false;false;false;false;false;false;false;true;true;true;true;true;false;false;false;false;false;false;false;false;false;false;false;false;false|] ;
   [|false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;true;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false|] ;
   [|false;false;false;false;false;false;false;false;false;false;false;false;false;false;true;false;true;false;false;false;false;false;false;false;false;false;false;false;false;false;false|] ;
   [|false;false;false;false;false;false;false;false;false;false;false;false;false;false;true;false;true;false;false;false;false;false;false;false;false;false;false;false;false;false;false|] ;
   [|false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;true;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false|] ;
   [|false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false|] ;
   [|false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false|] ;
   [|false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false|] ;
   [|false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false|] ;
   [|false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false|] ;
   [|false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false|] ;
   [|false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false|] ;
   [|false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false|] ;
   [|false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false|] ;
   [|false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false;false|]
 |];;