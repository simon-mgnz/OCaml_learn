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
    
(* Exercice 4 *)

let f x = let y = ref x and z = ref 0 and k = ref 1 in
  while !y > 0 do
    z:= !z + if !y mod 2 = 1 then !k else 0;
    y:= !y/2;
    k:= 2 * !k
  done;
  !z;;

(* Exercice 5 *)

let euler_qui_marche_pas n = 
  let v = ref 0 in
  for k=1 to n do 
    let u = ref k in
    while !v>0 do
      v := !u ;
      u:= !v mod !u
    done;
    if !v = 0 then incr v;
  done;
  !v;;


let euler n =
  let compteur = ref 0 in
  for k = 1 to n do
    let u = ref k in
    let v = ref n in
    while !u <> 0 do
      let r = !v mod !u in
      v := !u;
      u := r
    done;
    if !v = 1 then incr compteur
  done;
  !compteur
;;

(* Exercice 6 *)
let bezout (n,p) =
  if p mod n = 0 then (0,0)
  else
    let u1 = ref 1 and v1 = ref 0
    and u2 = ref 0 and v2 = ref 1 in
    let a = ref n and b = ref p in

    while !b <> 0 do
      let q = !a / !b in
      let r = !a mod !b in

      let u = !u1 - q * !u2 in
      let v = !v1 - q * !v2 in

      a := !b;
      b := r;

      u1 := !u2;
      u2 := u;

      v1 := !v2;
      v2 := v;
    done;

    (!u1, !v1)
;;
