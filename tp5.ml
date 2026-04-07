(* Exercice 1 *)
let rec carre liste = match liste with
  | [] -> []
  | hd::tl -> (hd *hd):: carre tl;;

let l = [1;2;3;4;5];;
carre l;;

let rec minimum2 liste = 
    match liste with
    | [] -> failwith "liste vide"
    | [x] -> x
    | hd::tl -> min hd (minimum2 tl);;

let rec minimum liste = 
    match liste with
    | [] -> failwith "liste vide"
    | [x] -> x
    | hd::tl -> let m = minimum tl in if hd < m then hd else m;;

print_int(minimum2 l);;

let rec somme liste1 liste2 =
  match (liste1,liste2) with
  | [],[] -> []
  | [], hd2::tl2 -> hd2:: somme [] tl2
  | hd1::tl1,[] -> hd1:: somme tl1 []
  | hd1::tl1,hd2::tl2 -> (hd1+hd2)::somme tl1 tl2 ;; 

let rec somme_correction l1 l2 =
  match (l1,l2) with
  | h1::tl1, h2::tl2 -> (h1+h2) :: somme tl1 tl2
  | _ -> [];;

let rec inverser li = match li with
  | [] -> []
  | hd::tl -> (inverser tl) @ [hd];;

let l2 = [1;2;3;4];;

let inverser2 l =
  let rec aux li acc = match li with
    | [] -> acc
    | hd::tl -> aux tl (hd::acc)
  in aux l [];;
inverser l2;;

let supprime liste = liste;;

