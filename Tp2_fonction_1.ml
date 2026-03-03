(* Exercice 1 *)

let f x y z = x+y+z;;
let f x y (z,t) u = x+y+z, t+u;;
let f x y = x y;;
let f x y z = z x + y;;
let f x y z = z (x + y);;
let f x y z = z * (x + y);; 


(* Exercice 2 *)

let test_egalite q p m n = (q * n == p * m) ;;

let test_egalite_correction (a,b) (c,d) = match (b*d, a*d-b*c) with
  | 0, _ ->  failwith "Dénominateur nul"
  |   _, 0 -> true 
  | _ -> false;;

(* Exercice 3 *)

let rec itere f u n = if n=0 then f u  else itere f u (n-1);;

let itere_juste f u n = 
  let g = ref u in
  for k = 1 to n do
    g := f !g done; g ;;
    
