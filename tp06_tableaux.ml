print_string("COUCOU");;

(* Exercice 1 *)
let reverse tableau =
  let n = Array.length tableau in
  match n with
  | 0 -> [||]
  | _ -> let nouveau_tableau = Array.make n tableau.(0) in
    for k=0 to n-2 do
      nouveau_tableau.(k) <- tableau.(n-1-k)
  done;
  nouveau_tableau;;

reverse [|1;2;3;4|];;

(* Exercice 2 *)
let reverse_bord tableau =
  let n = Array.length tableau in
  (* On s'en fou du cas [||]*)
  for k=0 to n/2 -1 do
    let temp = tableau.(k) in
      tableau.(k) <- tableau.(n-1-k);
      tableau.(n-1-k)<- temp;
    done;;

let t = [|1;2;3;4|];;
reverse_bord t;;
t;;

(* Exercice 3 *)

let palindrome str = 
  let compteur = ref 0 in
  let palind = ref true in
  let n = String.length str in
  while (!compteur <> n/2) && (!palind) do
    incr compteur;
    palind:= (str.[!compteur] = str.[n-1-(!compteur)])
  done;;

(palindrome "hello");;
palindrome "amogussugoma";;

let palindrome_correction s =
  let n = String.length s in
  let n2 = n/2 in
  let rec aux i = match i with
    | j when j >= n2 -> true
    | _ -> s.[i] = s.[n-1-i] && aux (i+1)
  in aux 0;;

(* Exercice 4 *)
let to_list s =
  let n = String.length s in
  let liste = ref [] in
  for k = n - 1 downto 0 do
    liste := s.[k] :: !liste
  done;
  !liste;;

let of_list l =
  let liste = ref l in
  let n = List.length l in
  let s = String.make n ' ' in 
  for i = 0 to n-1 do
    let e = List.hd !liste in
    (*s.[i] <- e;  Erreur*)
    liste := List.tl !liste
  done;
  s;;

(* Exercice 5 *)
let tribulle l = 
  let n = List.length l in
  let liste = ref l in
  let res = ref [] in
  for i = 0 to n-1 do
    let e = List.hd !liste in
    res := e :: !res;
    liste := List.tl !liste
  done;
  List.rev !res;;

tribulle [1;10;5];;

(* Exercice 6 *)
let triinsertion l =
  let n = List.length l in
  let liste = ref l in
  let res = ref [] in
  for i = 0 to n-1 do
    let e = List.hd !liste in
    let rec aux li acc = 
      match li with
      | [] -> List.rev (e::acc)
      | hd::tl when hd < e -> aux tl (hd::acc)
      | hd::tl -> List.rev_append acc (e::hd::tl)
      (* On trouve la première valeur plus grande que e, on insère e avant et on remet le reste de la liste après *)
    in res := aux !res [];
    liste := List.tl !liste
  done;
  List.rev !res;;

triinsertion [5;2;9;1;5;6];;
(* Pas propre du tout, on fait une boucle de fonctions récursives *)

(* Exercice 7 *)
(* On a le droit qu'a String.length et String.sub et Char.code*)
let longueur_caractere c =
  let code = Char.code c in
  if code land 0b10000000 = 0 then 1
  else if code land 0b11100000 = 0b11000000 then 2
  else if code land 0b11110000 = 0b11100000 then 3
  else if code land 0b11111000 = 0b11110000 then 4
  else failwith "octet invalide";;

let longueur_chaine s =
  let n = String.length s in
  let rec aux i count =
    if i >= n then count
    else
      let k = longueur_caractere s.[i] in
      aux (i + k) (count + 1)
  in
  aux 0 0;;

longueur_chaine "hello";;
longueur_chaine "€a€";;

(* La fonction précédente est de compléxité O(1) et ne dépend pas du nombre de caractères, tandis que celle-ci est de complexité O(n), elle dépend de la longueur de la chaine *)

let get i chaine =
  match i with
  | i when i <0 || i>= (String.length chaine) -> failwith "Index hors limites"
  | _ -> chaine.[i];;

(* Elle est de compléxité O(1) aussi, elle ne dépend pas de la longueur de la chaine, on accède directement à l'index i *)

let palindrome_le_retour_unicode s =
  let n = String.length s in
  let rec aux i =
    if i >= n / 2 then true
    else
      let c1 = get i s in
      let c2 = get (n - 1 - i) s in
      if c1 <> c2 then false
      else aux (i + 1)
  in aux 0;;
palindrome_le_retour_unicode "€a€";;