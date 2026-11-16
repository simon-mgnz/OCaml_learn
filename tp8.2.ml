let est_hamiltonien g =
  let n = Array.length g in 
  let visited = Array.make n false in
  let rec aux1 s nb = match s,nb with
    | s,_ when visited.(s) -> [] (* si sommet déjà visité *)
    | _,1 -> if List.mem 0 g.(s) then [s;0] else []
    | _ -> visited.(s) <- true; (* le sommet devient visité 🖒 *)
        match aux2 g.(s) (nb-1) with
        | [] -> visited.(s) <- false ; []
        | li -> s::li
  and aux2 li nb = match li with
    | [] -> []
    | hd :: tl -> match aux1 hd nb with
      | [] -> aux2 tl nb
      | l -> l
  in if n<=2 then [] else aux1 0 n;;
  
  
let g1=[|[1;4];[0;2;3;4;5];[1;3;4];[1;2;5];[0;1;2];[1;2;4]|];;
est_hamiltonien g1;;

let chavtal = [|
  [1; 2; 3; 4];
  [0; 7; 10; 11];
  [0; 6; 9; 11];
  [0; 8; 9; 11];
  [0; 5; 8; 10];
  [4; 6; 7; 11];
  [2; 5; 8; 10];
  [1; 5; 8; 9];
  [3; 4; 6; 7];
  [2; 3; 7; 10];
  [1; 4; 6; 9];
  [1; 2; 3; 5];|];;

est_hamiltonien chavtal
