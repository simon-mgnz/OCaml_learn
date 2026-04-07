(* Prérequis *)
type 'a arbre_binaire = Vide | N1 of 'a * 'a arbre_binaire * 'a arbre_binaire;;

type 'a arbre = F of 'a | N2 of 'a * 'a arbre * 'a arbre;;

type 'a arbre_naire = N3 of 'a * 'a arbre_naire list;;

(* Exercice 1 *)
let rec hauteur arbre =
  match arbre with
  | Vide -> -1
  | N1(a,b,c) -> max (hauteur b) (hauteur c) + 1;;

let rec taille arbre =
  match arbre with
  | Vide -> 0
  | N1(a,b,c) -> 1 + taille b + taille c;;

let rec hauteur_2 arbre =
  match arbre with
  | F(a) -> 0
  | N2(a,b,c) -> 1 + max (hauteur_2 b) (hauteur_2 c);;

let rec taille_2 arbre =
  match arbre with
  | F(a) -> 1
  | N2(a,b,c) -> 1 + taille_2 b + taille_2 c;;

let rec taille_3 arbre =
  match arbre with
  | N3(_,[])->1
  | N3(x, [b])-> 1+ taille_3 b
  | N3(x,hd::tl)-> taille_3 hd + taille_3 (N3(x,tl));;

let rec hauteur_3 arbre =
  match arbre with
  | N3(_,[]) -> 0
  | N3 (x,[b]) -> 1 + hauteur_3 b
  | N3(x, hd::tl)-> max (1+hauteur_3 hd)( hauteur_3 (N3(x,tl)));;

(* Exercice 2 *)
let rec branche_droite arbre =
  match arbre with
  | Vide -> []
  | N1(a,bd,_) -> a :: (branche_droite bd) ;;

let rec branche_droite_quelconque arbre =
  match arbre with
  | N3(_) -> []
  | N3(x,l) -> List.hd l :: (branche_droite_quelconque arbre);;

let rec feuilles arbre =
  match arbre with
  | Vide -> 1
  | N1(a,b,c) -> feuilles b + feuilles c
(*
let rec feuilles_quelconques arbre =
  match arbre with
  | N3()*)