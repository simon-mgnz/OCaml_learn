(* Exercice 1 *)
type complexe = Alg of float * float | Exp of float * float;;

let pi = 3.1415;;

let conversion z = 
  match (x,y) with
  | Exp(r,t) -> Alg(r *. cos(t),r *. sin(t))
  | _ -> z;;

let rec somme x y = match (x,y) with
  | Alg(a,b),Alg(c,d) -> Alg(a+.c,b+.d)
  | _ -> somme (conversion x) (conversion y);;

let rec multiplication x y = match (x,y) with
  | Exp(r1,t1), Exp(r2,t2) -> Exp(r1 *. r2, t1 +.t2)
  | Alg(a,b),Alg(c,d) -> Alg(a*.c-.b*.d,a*.d+.b*.c)
  | _ -> produit (conversion x) (conversion y);;

let rec modus x = 
  match x with
  | Alg(a,b) -> sqrt(a**2+b**2)
  | Exp(r,t) -> r
  | _ -> modus (conversion x);;

let rec inverse x =
  match x with
  | Alg(a,b) when a <>0 && b<> 0-> Alg(1/.a,1/.b)
  | Alg(a,b) -> failwith "Division par zéro !"
  | Exp(r,t) when r<>0 -> Exp(1/.r,-t)
  | Exp (r,t) -> failwith "Division par zéro !"
  | _ -> inverse (conversion x);;

type complexe2 = {re: float; im : float};;
let z = {re =2.0; im = 3.0};;
let x = Alg(2., 3.);
let y = Exp (1,0, (acos(-1.)));;

let somme x y =
  {re = x.re +. y.re; im = x.im +. y.im};;

let somme2 x y =
  match (x,y) with
  | {re = a; im= b}, {re = c; im =d} -> {re = a+. c; im = b +. d};;

let puissance x n =
  match (x,n) with
  | (x,n) -> 

let inverse x = match x with
| 0 -> None
| _ -> Some(1. /. float_of_int x);;