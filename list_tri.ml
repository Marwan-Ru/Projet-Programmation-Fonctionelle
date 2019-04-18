(* module list tri *)

(* fonctions de comparaison *)
(*
let croissant x y = (x <= y) ;;
let decroissant x y = (x >= y) ;;
*)

(* Tri pivot *)

(* 1. *)
(*
let rec partitionne_pivot_bis croissant l pivot l1 l2 =
match l with
[]-> (l1, l2)
|x::r -> if (croissant x pivot)
         then partitionne_pivot_bis croissant r pivot (x::l1) l2
         else partitionne_pivot_bis croissant r pivot l1 (x::l2) ;;
*)

let partitionne_pivot croissant l  =
match l with 
[] -> ([],[]) 
|x::r -> partitionne_pivot_bis croissant l x [] [] ;;

(*
let list = random_list 100 20 ;;
partitionne_pivot croissant list ;;
*)

(* 2. *) 

let rec tri_pivot croissant l = 
let (l1, l2) = partitionne_pivot croissant l in
match l1,l2 with 
([],[]) -> []
|([], x::(y::r)) -> if (croissant x y)
                    then x::(tri_pivot croissant (y::r))
                    else y::(tri_pivot croissant (x::r))
|([], x::r) -> x::(tri_pivot croissant r)
|(x::(y::r),[]) -> if (croissant x y)
                   then x::(tri_pivot croissant (y::r))
                   else y::(tri_pivot croissant (x::r))
|(x::r,[]) -> x::(tri_pivot croissant r)
|(x1::y1::r1,x2::y2::r2) -> if (croissant x y)
                    then x::(tri_pivot croissant (y::r2))
                    else y::(tri_pivot croissant (x::r1));;

tri_pivot croissant [4;2;6;1;3;9;5] ;;	      



























































































