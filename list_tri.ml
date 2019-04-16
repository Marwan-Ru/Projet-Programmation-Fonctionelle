(* module list tri *)

(* fonctions de comparaison *)

let croissant x y = (x <= y) ;;
let decroissant x y = (x >= y) ;;

(* Tri pivot *)

(* 1. *)

let partitionne_pivot_bis croissant l pivot l1 l2 =
match l with
[]-> (l1, l2)
|x::r -> if (croissant x pivot)
         then (x::l1,l2)
         else (l1, x::l2) ;;

let partionne_pivot croissant l pivot =
partitionne_pivot_bis croissant l pivot [] [] ;;

(* 2. *) 


