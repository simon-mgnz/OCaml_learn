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

let to_list s =
  let n = String.length s in
  let liste = [] in
  for k=0 to n-1 do
    match liste with
    | hd::tl -> hd::tl::s.[k]
  done;
  liste;;

let of_list l =
  Hexahello.hello;;
  