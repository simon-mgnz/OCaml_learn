type graphe = int list array;;
type color = Sarcele | Cuivre | Gris;;

let change_couleur c = match c with
  | Sarcele -> Cuivre
  | Cuivre -> Sarcele
  | _ -> Gris;; 

let estbiparti g =
  let n = Array.length g in
  let couleurs = Array.make n Gris in
  let rec prochain_incolore i = match i with
    | j when j = n -> -1
    | _ -> prochain_incolore (i+1) in 
  let colorie s c = match couleur.(s) with
    | Gris -> couleurs.(s) <- c; colorie_voisin g.(s) (change_couleur c)
    | d when d = c -> true 
    | _ -> false
  and colorie voisins li c = match li with
    | [] -> true
    | hd::tl -> colorie hd && colorie_voisins tl in;;
(*Pas fini, par récursivité...*)
