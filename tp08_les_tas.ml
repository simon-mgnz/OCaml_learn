(* PARTIE 1 : Préliminaires
1. n impair

2. 2**k -1 < n <= 2**(k+1) -1
2*k <= n < 2**(k+1)
h<= log_2(n)<h+1
ssi h = floor (log_2(n)) 

3. Layer : floor(log_2 (i))

Position du noeud : floor(2**(log_2(i) - floor(log_2(i))))+1
Fils de gauche : Position du noeud * 3 + 1
Fils de droite : Fils de gauche + 1
Son papa : Position du noeud //2 - 2**layer

4. Si le tableau est trié dans l'ordre décroissant :
Tous les fils sont inférieur à leurs pères. D'ou le i.
C'est possible qu'il soit persque complet, ce n'est pas déterminé par la liste puisque toutes les valeurs ne forcent pas cela, (elles sont décroissantes).
La réciproque est évidement fausse ! Un tas n'est pas forcément trié, cf exemple.

5.             100
         78           56
      66     33     57   31
    44  19  18  25 6          
Ce n'est pas un tas à cause valeurs qui varient entre grands et petits...
*)

(* PARTIE 2 : Tamissage et Tassage*)