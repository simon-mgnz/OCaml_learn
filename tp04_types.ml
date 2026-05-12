(* Ceci est un éditeur pour OCaml
   Entrez votre programme ici, et envoyez-le au toplevel en utilisant le
   bouton "Évaluer le code" ci-dessous ou [Ctrl-e]. *)

type complexe =  {re : float ; im : float};;

(*
  let conversion x = {
    re = x.r *. cos z.t;
    im = x.r *. sin z.t;
  };;
*)



let plus x y = {
  re = x.re +. y.re;
  im = x.im+.y.im;
};;


let fois x y = {
  re = (x.re *. y.re)-.(x.im *. y.im);
  im = (x.im*.y.re)+.(x.re *. y.im);
};;

let opp x = 
  {
    re = -.x.re;
    im = -.x.im;
  };;

let inv x = 
  if x.re = 0. && x.im=0. then failwith "DivisionParZero"
  else
    let norm = x.re *. x.re +. x.im *. x.im in
    { re = x.re /. norm;
      im = -. x.im /. norm };;

let moins x y =
  {
    re = x.re -. y.re;
    im = x.im-.y.im
  };;


let division x y =
  fois x (inv y);;

let modules x = 
  {
    re = x.re;
    im = -. x.im
  };;

let puissance x n =
  if n mod 4 = 0 then
    {
      re = x.re ** n;
      im = x.im **n
    }
  else if  n mod 4 = 1 then
    {
      re = x.re ** n+. x.im**n;
      im = 0
    }
  else if n mod 4 = 2 then
    {
      re = x.re ** n;
      im = -. (x.im **n)
    }
  else
    {
      re = x.re ** n -. x.im**n;
      im = 0
    };;

let expo x =
  {
    re = (exp x.re) *. (cos x.im);
    im = (exp x.re) *. (sin x.im)
  };;


let to_alg = function
  | Alg (a, b) -> Alg (a, b)
  | Exp (r, theta) ->
      Alg (r *. cos theta, r *. sin theta)

let to_exp = function
  | Exp (r, theta) -> Exp (r, theta)
  | Alg (a, b) ->
      let r = sqrt (a *. a +. b *. b) in
      let theta = atan2 b a in
      Exp (r, theta);;

let rec add2 z1 z2 =
  match z1, z2 with
  | Alg (a, b), Alg (c, d) ->
      Alg (a +. c, b +. d)
  | _ ->
      add2 (to_alg z1) (to_alg z2);;


let rec opp2 = function
  | Alg (a, b) -> Alg (-.a, -.b)
  | Exp (r, theta) -> opp2 (to_alg (Exp (r, theta)));;
                        
let sub z1 z2 =
  add z1 (opp z2);;

let rec mul z1 z2 =
  match z1, z2 with
  | Exp (r1, t1), Exp (r2, t2) ->
      Exp (r1 *. r2, t1 +. t2)
  | _ ->
      mul (to_exp z1) (to_exp z2);;

exception DivisionParZero

let est_nul = function
  | Alg (a, b) -> abs_float a < 1e-10 && abs_float b < 1e-10
  | Exp (r, _) -> r < 1e-10;;

let rec inv2 z =
  if est_nul z then raise DivisionParZero
  else
    match z with
    | Exp (r, theta) ->
        Exp (1. /. r, -.theta)
    | _ ->
        inv2 (to_exp z);;

let rec pow z n =
  if n = 0 then Exp (1., 0.)
  else if n < 0 then
    pow (inv z) (-n)
  else if n mod 2 = 0 then
    let half = pow z (n / 2) in
    mul half half
  else
    mul z (pow z (n - 1));;

let div2 z1 z2 =
  mul z1 (inv z2)
    
let rec exp_complexe z =
  match z with
  | Alg (a, b) ->
      let r = exp a in
      Exp (r, b)
  | Exp _ ->
      exp_complexe (to_alg z)


type complexe2 = Alg of float * float | Exp of float * float;;

let inverse n =
  if n = 0 then None
  else Some (1. /. float_of_int n)

let racine x =
  if x < 0. then None
  else Some (sqrt x)

(* A corriger *)