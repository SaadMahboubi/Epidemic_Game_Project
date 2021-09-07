(*
Auteur :            Saad MAHBOUBI -685L 
                    Erwann MOREAU -685K
*) 

(*------------------------------DEFINITION DES TYPES--------------------------------*)
(* ensemble fini de tous les états possibles *)
type etatPossible = 
  MortOuImmunise
| Infecte 
| Sain;;
type position = {x : int ; y : int};;
type cellule = {pos : position ; etat : etatPossible};;
type listeDePositions = position list;; 
type listeDeCellules = cellule list;; 
type monde = {dep : cellule ; listeContagieux : listeDeCellules ; listePlusContagieux : listeDeCellules ; listeInfecte : listeDeCellules};;

(*------------------DEFINITION DES CONSTANTES ET VARIABLES---------------------------*)
(*initiliser la premiere cellule qui sera infecté, la listeContagieux cotient les cellule qui sont mort ou immunisé et qui peuvent infecté un voisin sain
et listePlusContagieux contient les cellules morte ou immunisé qui ne peuvent plus infecté*)
let positionDepart = {x = 0 ; y = 0}
let depart = {pos = positionDepart ; etat = Infecte};;
let monde = {dep = depart ; listeContagieux = [] ; listePlusContagieux = [] ; listeInfecte = [depart]};;
(*coordonnées des cellules adjacentes*)
let voisins = [[-1;-1];[-1;0];[-1;1];[0;1];[0;-1];[1;1];[1;0];[1;-1]];;

(*----------------------------DEFINITION DES FONCTIONS-------------------------------*) 


(*
val retourneEtat : position -> monde -> etatPossible = <fun>
Fonction qui parcourt le monde (les liste) afin de connaitre l'état de la cellule donné*)
let retourneEtat pos monde =
let rec auxRetourneEtat3 pos listeInfecte =
  match listeInfecte with
  | a::b -> if a.pos=pos then Infecte else auxRetourneEtat3 pos b
  | [] -> Sain
in let rec auxRetourneEtat2  pos listePlusContagieux =
     match listePlusContagieux with
     | a::b -> if a.pos=pos then MortOuImmunise else auxRetourneEtat2 pos b
     | [] -> auxRetourneEtat3 pos monde.listeInfecte
in let rec auxRetourneEtat1 pos listeContagieux = 
     match listeContagieux with
     | a::b -> if a.pos=pos then MortOuImmunise else auxRetourneEtat1 pos b
     | [] -> auxRetourneEtat2 pos monde.listePlusContagieux
in auxRetourneEtat1 pos monde.listeContagieux;;




(*
val deplacer : position -> int list -> position = <fun>
fonction qui nous permet de se deplacer de la position pos1 avec les coordonné de la pos2*)
let deplacer pos1 coord = 
{x = pos1.x + (List.nth coord 0) ; y = pos1.y + (List.nth coord 1)};; 


(*
val recupPosVoisines :
cellule -> int list list -> position list -> position list = <fun>
Fonction qui recupère toutes les positions voisines a la cellule donné en paramètre et les stock dans une liste de positions*)
let rec recupPosVoisines cellule voisins listePosVoisins2 = 
match voisins with
| a::b -> recupPosVoisines cellule b ((deplacer cellule.pos a)::listePosVoisins2)
| [] -> listePosVoisins2
;;

(*
val recupCellulesVoisines : cellule -> int list list -> monde -> cellule list = <fun>
Fonction qui recupere toutes les cellules voisines a la cellule donné en paramètre et les stock dans une liste de cellules*)
let recupCellulesVoisines cellule voisins monde =
let posVoisines = recupPosVoisines cellule voisins [] in
let rec auxRecupCellulesVoisines cellule posVoisines monde listeCelluleVoisines = 
  match posVoisines with
  | a::b -> auxRecupCellulesVoisines cellule b monde ({pos = a ; etat = retourneEtat a monde}::listeCelluleVoisines)
  | [] -> listeCelluleVoisines
in auxRecupCellulesVoisines cellule posVoisines monde []
;;


(*
val peuventContamineVoisins : cellule list -> monde -> int -> bool = <fun>
Fonction qui verifie si la cellule en paramètre a au moins une cellule saine comme voisines
si oui on retourne True, False sinon
*) 
let peuventContamineVoisins listeCibles monde =
let rec auxPeuventContamineVoisins listeCibles monde compteur =
  match listeCibles with
  | a::b -> if (a.etat = MortOuImmunise) 
      then auxPeuventContamineVoisins b monde (compteur+1)
      else auxPeuventContamineVoisins b monde compteur
  | [] -> if (8>compteur) then true else false
in auxPeuventContamineVoisins listeCibles monde 0
;;

(*
val concat : 'a list -> 'a list -> 'a list = <fun>
fonction qui concatene deux liste
*) 
let rec concat l1 l2 = 
match l1 with
  [] -> l2
| p:: r -> p:: concat r l2
;;

(*
val supprimerElemListe : 'a -> 'a list -> 'a list = <fun>
fonction qui supprime l'element en parametre de la liste
*) 
let supprimerElemListe cell liste = 
let rec auxSupprimerElemListe cellule liste listeFinal =
  match liste with
  |a::b -> if(a=cellule) then (concat listeFinal b)
      else auxSupprimerElemListe cellule b (a::listeFinal)
  |[] -> listeFinal
in auxSupprimerElemListe cell liste []
;;

(*
val ajoutDansContagieux : cellule -> monde -> listeDeCellules -> monde =
<fun>
Ces deux fontions nous permettent d'ajouter la cellule dans le monde à la liste associé
et en changeant l'état de la cellule
*) 
let ajoutDansContagieux cell monde listeInf = 
let nVmonde = {dep = {pos = positionDepart ; etat = MortOuImmunise}; listeContagieux = {pos = cell.pos ; etat = MortOuImmunise}::monde.listeContagieux ; listePlusContagieux = monde.listePlusContagieux ; listeInfecte = listeInf} in
nVmonde
;;
let ajoutDansPlusContagieux cell monde listeInf = 
let nVmonde = {dep = {pos = positionDepart ; etat = MortOuImmunise}; listeContagieux = (supprimerElemListe cell monde.listeContagieux) ; listePlusContagieux = {pos = cell.pos ; etat = MortOuImmunise}::monde.listePlusContagieux ; listeInfecte = listeInf} in
nVmonde
;; 

(*
val ajoutDeContagieuAPlusContagieux : cellule -> monde -> monde =
<fun>
Fontion nous permettent d'ajouter la cellule qui était contagieuse dans la liste des cellules plus contagieuse
*) 
let ajoutDeContagieuAPlusContagieux cell monde = 
let nVmonde = {dep = {pos = positionDepart ; etat = MortOuImmunise}; listeContagieux = (supprimerElemListe cell monde.listeContagieux) ; listePlusContagieux = {pos = cell.pos ; etat = cell.etat}::monde.listePlusContagieux ; listeInfecte = monde.listeInfecte} in
nVmonde
;;

(*
val seraInfecte : bool -> bool = <fun>
Fonction qui prend en parametre vrai et retourne un bool, 
on a une chance sur 4 de tomber sur vrai si c'est le cas on retourne vrai faux sinon
(25% de chance d'être infecté) 
*)
Random.self_init();;
let seraInfecte flag =
let random = Random.int 4 in
if random=1 then
  flag = true
else
  flag = false 
;;

(*
val ajoutDansInfecte : monde -> cellule list -> monde = <fun>
Fonction infecte les voisins de la cellule.
*)
let rec ajoutDansInfecte monde listVoisins = 
match listVoisins with
|a::b ->
    if ((seraInfecte true) && a.etat = Sain) then
      begin
        let nVmonde = {dep = {pos = positionDepart ; etat = MortOuImmunise}; listeContagieux = monde.listeContagieux ; listePlusContagieux = monde.listePlusContagieux; listeInfecte = {pos = a.pos ; etat = Infecte}::monde.listeInfecte} in
        ajoutDansInfecte nVmonde b
      end
    else
      ajoutDansInfecte monde b
|[] -> monde
;; 


(*
val infecteDevientMortOuImmunise : int list list -> monde -> monde = <fun>
Fonction qui Transforme les cellule infecté en cellule Morte ou (Immunisé début de chaque étape i pour i>0)
*) 
let infecteDevientMortOuImmunise voisins monde = 
let rec aux voisins monde listeInfect =
  match listeInfect with 
  | a::b -> aux voisins (ajoutDansContagieux a monde b) b
  | [] -> monde
in aux voisins monde monde.listeInfecte
;;

(*
val contagieuxDevientPlusContagieux : int list list -> monde -> monde = <fun>
Fonction qui met les élément mort qui n'ont plus de cellule a infecté dans la liste corréspodante
*) 
let contagieuxDevientPlusContagieux voisins monde = 
let rec auxContagieuxDevientPlusContagieux voisins monde listeContagieux =
  match listeContagieux with 
  | a::b -> if (peuventContamineVoisins (recupCellulesVoisines a voisins monde) monde)
      then 
        auxContagieuxDevientPlusContagieux voisins monde b
      else
        auxContagieuxDevientPlusContagieux voisins (ajoutDeContagieuAPlusContagieux a monde) b
  | [] -> monde
in auxContagieuxDevientPlusContagieux voisins monde monde.listeContagieux
;;
    

(*
val infection : int list list -> monde -> monde = <fun>
Fonction qui Transforme les cellule saines en cellule infecte : 
La cellule infectée au temps i meurt ou devient immunisée au coup suivant (i+1)
*) 
let infection voisins monde =
let rec auxInfection voisins monde listContag =
  match listContag with
  | a::b -> 
      if (peuventContamineVoisins (recupCellulesVoisines a voisins monde) monde)
      then
        auxInfection voisins (ajoutDansInfecte monde (recupCellulesVoisines a voisins monde)) b
      else
        auxInfection voisins (ajoutDeContagieuAPlusContagieux a monde) b
  |[] -> monde
in auxInfection voisins monde monde.listeContagieux
;;