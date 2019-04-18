(* module list tri *)

(* fonctions de comparaison *)

let croissant x y = (x <= y) ;;
let decroissant x y = (x >= y) ;;

(* Tri pivot *)

(* 1. *)

let rec partitionne_pivot_bis croissant l pivot l1 l2 =
match l with
[]-> (l1, l2)
|x::r -> if (croissant x pivot)
         then partitionne_pivot_bis croissant r pivot (x::l1) l2
         else partitionne_pivot_bis croissant r pivot l1 (x::l2) ;;

let partitionne_pivot croissant l  =
partitionne_pivot_bis croissant l (List.hd l) [] [] ;;

let list = random_list 100 20 ;;
partitionne_pivot croissant list ;;

(* 2. *) 


