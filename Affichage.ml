(*
Auteur :            Saad MAHBOUBI -685L
*) 
open Graphics
open Automatecell


(*Choix d'une taille fixe de la grille et des cellules*)
let width_size  = 900;;
let height_size = 900;;
let rect_size = 100;;

(* val estDansLaListe : cellule liste -> int -> int -> bool = <fun>
Fonction qui retourne vrai si la position de coordonnées x y est présent ou non dans la liste*)
let rec estDansLaListe liste x y =
  match liste with
  | a::b -> if((a.pos.x = x) && (a.pos.y = y)) then true else estDansLaListe b x y
  | [] -> false
;;

(* val grille : monde -> grille = <fun>
Fonction qui dessine la grille*)
let grille monde =
    for i = -4 to 4 do
        for j = -4 to 4 do
            if (estDansLaListe monde.listeInfecte i j) then set_color red 
            else (if ((estDansLaListe monde.listeContagieux i j) || (estDansLaListe monde.listePlusContagieux i j)) then set_color green 
            else set_color white);
            fill_rect ((rect_size * i)+400) ((rect_size * j)+400) rect_size rect_size;
            set_color black;
            draw_rect ((rect_size * i)+400) ((rect_size * j)+400) rect_size rect_size
        done
    done
;;

open_graph (Printf.sprintf " %ix%i" width_size height_size);; (* espace obligatoire avant le %i *)


(* val jouerInfection : monde -> monde = <fun>
Fonction qui joue le monde et appel la grille et a chaque appuie sur une touche de clavier l'algorithme passe au monde suivant*)
let rec jouerInfection monde = 
  grille monde;
  let e = Graphics.wait_next_event [Graphics.Key_pressed] in
    if e.Graphics.keypressed then
    jouerInfection (jouer monde 1)
;;

(*Initialisation du monde et appel de la fonction principale*)
let positionDepart = {x = 0 ; y = 0} in
let depart = {pos = positionDepart ; etat = Infecte} in
let nvMonde = {dep = depart ; listeContagieux = [] ; listePlusContagieux = [] ; listeInfecte = [depart]} in
jouerInfection nvMonde;;

ignore (wait_next_event [ Key_pressed ]);;
