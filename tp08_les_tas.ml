(* PARTIE 1 : Préliminaires
1. n impair

2. 2**k -1 < n <= 2**(k+1) -1
2*k <= n < 2**(k+1)
h<= log_2(n)<h+1
ssi h = floor (log_2(n)) 

3. Layer : floor(log_2 (i))

Position du noeud : floor(2**(log_2(i) - floor(log_2(i))))+1
Fils de gauche : Position du noeud * 3 + 1
Fils de droite : Fils de gauche + 1
Son papa : Position du noeud //2 - 2**layer

4. Si le tableau est trié dans l'ordre décroissant :
Tous les fils sont inférieur à leurs pères. D'ou le i.
C'est possible qu'il soit persque complet, ce n'est pas déterminé par la liste puisque toutes les valeurs ne forcent pas cela, (elles sont décroissantes).
La réciproque est évidement fausse ! Un tas n'est pas forcément trié, cf exemple.

5.             100
         78           56
      66     33     57   31
    44  19  18  25 6          
Ce n'est pas un tas à cause valeurs qui varient entre grands et petits...
*)

(* PARTIE 2 : Tamissage et Tassage*)

(* Question 6
Si le noeud gauche est supérieur à la racine et au noeud droite:
  Si le noeud droite est supérieur à la racine: 
    Passer la valeur du noeud gauche à la racine, le noeud droite au noeud gauche et la racine au noeud droite
  Sinon :
    Echanger la valeur du noeud gauche avec la racine
Sinon si le noeud droite est supérieur à la racine:
  Echanger la valeur du noeud droite à la racine.
*)

let rec tamiser t i =
  let parent = t.(i) in
  if Array.length t < i*3+2 then
  let fils_gauche, fils_droit = t.((i*3)+1),t.((i*3)+2) in
  match fils_gauche> parent && fils_gauche > fils_droit with
  | true when fils_droit>parent -> t.(i) <- fils_gauche ;
            t.(i*3+1) <- fils_droit;
            t.(i*3+2) <- parent;
            tamiser t (i+1)
  | true -> t.(i) <- fils_gauche;
            t.(i*3+1) <- parent;
            tamiser t (i+1)
  | _ when fils_droit > parent -> t.(i) <- fils_droit;
                                  t.(i*3+2) <- parent;
                                  tamiser t (i+1)
  |_ -> tamiser t (i+1)
  

let tamiser_correction t i =
  let n = Array.length t in
  let rec aux j = match j with
  | j when 2*j+1 >= n -> () (* feuille *)
  | k when 2*k+1 = n-1 -> (* un fils gauche *)
    if t.(j) < t.(2*j+1) then let temp = t.(j) in
      t.(j) <- t.(2*j+1);
      t.(2*j+1) <- temp
  | _-> (* 2 fils *)
    let m=max t.(2*j+1) t.(2*j+2) in
    if t.(j)<m then begin
      let k = if m = t.(2*j+1) then 2*j+1 else 2*j+2 in
        t.(k)<- t.(j);
        t.(j) <- m;
        aux k;
      end;
  in aux i;;

(* Complexité O(h) = O(log(n)) (h est la hauteur de l'arbre)*)

(* Question 7 *)
let tasser t =
  for k=Array.length t downto 1 do
    tamiser t k done;;
(* Complexité : n*O(log(n)) = O(n*log(n))*)

(* PARTIE 3 : Tri par tas *)

(* Question 8 *)