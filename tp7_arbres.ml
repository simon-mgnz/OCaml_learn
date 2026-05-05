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


(* Exercice 3 *) 
let rec feuilles arbre =
  match arbre with
  | Vide -> 1
  | N1(a,b,c) -> feuilles b + feuilles c;;

let rec feuilles_quelconques arbre =
  match arbre with
  | N3(x,[]) -> [x]
  | N3(a,x) -> let rec f y =
                 match y with
                 | [] -> []
                 | hd::tl -> (feuilles_quelconques hd) @ (f tl)
      in f x;;

(*Exercice 4*)
let sosa arbre = 
  let rec aux i arbre = 
    match arbre with
    | F(x) -> F i
    | N2(a,g,d)-> N2(i, aux (i*2) g, aux (i*2 +1) d)
  in aux 1 arbre;;

(* Exerice 5 *)
let profondeur arbre =
  let rec aux a compteur =
    match a with
    | Vide -> [compteur]
    | N1(a,d,g)-> (aux d (compteur+1)) @ (aux g (compteur+1))
  in aux arbre 1;;
let pop liste =
  match liste with
  |[] -> failwith "Vide"
  |hd::tl-> hd, tl;;

let arbre_complet_1 arbre =
  let valeur = ref (List.hd (profondeur arbre)) in
  let l = profondeur arbre in
  let resultat = ref true in
  let rec aux liste =
    match liste with
    | [] -> !resultat
    | hd::tl -> if !valeur <> hd then resultat:= false; aux tl
  in aux l;;

let ar= N1(2,N1(0,Vide,Vide),N1(3,N1(1,Vide,Vide),Vide));;

profondeur ar;;
arbre_complet_1 ar;;

let arbre_complet_2 a =
  let prof = profondeur a in
  let ht = hauteur a in
  let valeur = ref true in
  let rec aux liste =
    match liste with
    | [] -> !valeur
    | hd::tl -> if hd<> ht || hd<>ht -1 then valeur := false; aux tl
    in aux prof;;

(* Exercice 6*)
let verifie_ABR arbre =
  let rec aux a mini maxi =
    match a with
    | Vide -> true
    | N1(x,g,d) -> if x <= mini || x >= maxi then false
    else (aux g mini x) && (aux d x maxi)
  in aux arbre min_int max_int;;

let cherche arbre x =  
  let rec aux a =
    match a with
    | Vide -> false
    |N1(y,g,d) -> if x = y then true else aux d || aux g
  in aux arbre;;

let rec insere arbre x =
  match arbre with
  | Vide -> N1(x,Vide,Vide)
  | N1(y,g,d) -> if x = y then arbre
  else if x < y then N1(y, insere g x, d)
  else N1(y,g,insere d x);;

  let  rec supprime arbre x = 
    match arbre with
    | Vide -> Vide
    | N1(y,g,d)-> if x < y then N1(y, supprime g x, d)
    else if x = y then Vide
    else N1(y, g, supprime d x);;