
(* Exercice 1 *)
let rec puissance x n = match n with
  | 0 -> 1
  | _ -> match n mod 2 with
    | 0 -> puissance (x*x) (n/2)
    | _-> x*puissance (x*x) (n/2);;


let terminale x m =
  let rec aux x n acc = match n with
    |0-> acc
    | _-> match n mod 2 with
      |0 -> aux (x*x) (n/2) acc
      | _ -> aux (x*x) (n/2) (acc*x) in aux x m 1;;

(* Exercice 2 *)
let rec puissancemod x n p = match n with
  |0 -> 1
  |_ -> match n mod 2 with
    |0 -> puissancemod(x*x mod p) (n/2) p
    | _ -> ((x mod p) * puissancemod ((x*x) mod p) (n/2) p) mod p;;

(* Exercice 3 *)
let floyd1 f a =
  let u_len = ref (f a) in
  let u_rap = ref (f (f a)) in
  while !u_rap <> !u_len do
    u_len:= f !u_len;
    u_rap:= f (f !u_rap)
  done;
  !u_len;;


let floyd2 f a =
  let u_len = ref (f a) in
  let u_rap = ref (f (f a)) in
  let compteur = ref 0 in
  while !u_rap <> !u_len do
    u_len:= f !u_len;
    u_rap:= f (f !u_rap);
    incr compteur
  done;
  !compteur;;


(* Exercice 4 - Ne fonctionne pas*)
let rec compte st =
  let c = st.[0] in
  let rec compte_debut i =
    if i < String.length st && st.[i] = c then compte_debut (i+1) else i
  in
  let k = compte_debut 0 in
  string_of_int k ^ String.sub st 0 1 ^
  compte (String.sub st k (String.length st - k));;

let decompte n =
  let comptage = ref "1" in
  let rec aux n comptage =
    match n with
    | 0 -> !comptage
    | _ -> comptage := compte !comptage;
        aux (n-1) !comptage
  in aux n !comptage;; 
