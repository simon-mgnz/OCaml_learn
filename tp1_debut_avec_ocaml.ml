(* Ceci est un éditeur pour OCaml
   Entrez votre programme ici, et envoyez-le au toplevel en utilisant le
   bouton "Évaluer le code" ci-dessous ou [Ctrl-e]. *)

(* Exercice 1 *)
let a = log 2.0 in (a+. 8.0)/. (5.0 -. 2.0*. a) ;;
let b = sqrt 5. in exp b +. 3. *. b /.7.;; 

(* Exercice 2 *)
3*8;;
cos 2.;;
tan 2.0 ;;
print_newline ();;

(* Exercice 3 *)
let a = let x = 2 and y = 3 in x + y + 2;;
let a = let x,y =2,3 in x*y;;
let a = let x,y = 2, false in  x = 2 || y
and b = let x,y,z=2,0.0, true in x > int_of_float y && z;;

(* Exercice 4
Attention aux références ! *)
let n = 1000 in 
for k=1 to n-1 do
  let somme = ref 0 in
  for l=1 to k/2 do 
    if k mod l = 0 then somme := !somme + k;
  done ;
  if !somme = k then begin
    print_int(k);
    print_newline();
  end
done;;

(* Exercice 5 *)
let n = 10000 in
let count = ref 0 in
let k = ref 0 in

while !count < n do
    (* calcul de la somme des carrés des chiffres *)
  let temp = ref !k in
  let somme = ref 0 in

  while !temp > 0 do
    let d = !temp mod 10 in
    somme := !somme + (d * d*d);
    temp := !temp / 10
  done;

  if !k = !somme then begin
    print_int !k;
    print_newline();
    incr count
  end;

  incr k
done

(* Exercice 6 *)
let n = 100 in
let u = ref 1.0 in

for k = 1 to n do
  u := sin !u;
  let denom = sqrt (3.0 /. float_of_int k) in
  let rapport = !u /. denom in
  print_float rapport;
  print_newline ()
done;;
