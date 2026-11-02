(* This is an OCaml editor.
   Enter your program here and send it to the toplevel using the "Eval code"
   button or [Ctrl-e]. *)


(* Td 6.2 Backtracking *)
let creer_entiers_disponibles p =
  Array.make (p+1) true;; 
    
  
let creer_debut_ligne n =
  let a = Array.make (n*(n+1)/2) false in
  for k=0 to n-1 do 
    a.(k*(k+1)/2) <- true;
  done;
  a;;

let creer_indice_ligne n =
  let a = Array.make (n*(n+1)/2) 1 in
  for k=2 to n do
    for j = k*(k-1)/2 to k*(k+1)/2-1 do
      a.(j) <- k;
    done;
  done;
  a;;

creer_debut_ligne 10;;
creer_indice_ligne 10;;

let resoudre n p =
  let pyramid = Array.make (n*(n+1)/2) 0
  in let disponible = creer_entier_disponibles p
  in let debut_ligne = creer_debut_ligne n
  in let indice_ligne = creer_indice_ligne n
         
  in let verif i = debut_ligne.(i) ||
                   abs ( pyramid.(i) - pyramid.(i-1)) = pyramid.(i-indice_ligne(i))
  
  in let prochain_dispo i = match i with 
      | j when j > p -> - 1 (* Condition d'exclusion *)
      | _ -> if disponible.(i) then i else prochain_dispo (i+1)
  in let placer i a = begin
      disponible.(pyramid.(i))<- true;
      disponible.(a) <- false;
      pyramid.(i) <- a;
    end in
  let rec aux i a = 
    match i, prochain_dispo a with
    | j,_ when j>= s -> true
    | _,-1 -> false
    | _,b -> placer i b; 
        if verif i then aux (i+1) 1 else (*pas fini *)
