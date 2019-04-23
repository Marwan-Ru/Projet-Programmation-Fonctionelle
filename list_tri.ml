(* module list tri *)

(* fonctions de comparaison *)

let croissant x y = (x < y) ;;

(*
let decroissant x y = (x >= y) ;;
*)

(* Tri pivot *)

(* 1. *)

let rec partitionne_pivot_bis croissant l pivot l1 l2 =
match l with
[]-> (l1,(pivot::l2))
|x::r -> if (croissant x pivot)
            then partitionne_pivot_bis croissant r pivot (x::l1) l2
            else partitionne_pivot_bis croissant r pivot l1 (x::l2) 

let partitionne_pivot croissant l  =
match l with 
[] -> ([],[]) 
|x::r -> partitionne_pivot_bis croissant r x [] [] ;;

(*
 let list = random_list 100 20 ;;
 *)

partitionne_pivot croissant [4;2;6;1;3;9;5] ;;


(* 2. *)


let rec tri_pivot croissant l =
if l = []
then []
else let (l1,l2) = partitionne_pivot croissant(List.tl l) in
if (List.length l1) = 1 && (List.length l2) = 1
then  if (croissant (List.hd l) (List.hd l1))
      then (List.hd l::[])@ l1 @ l2
      else if (croissant (List.hd l) (List.hd l2))
           then l1 @ (List.hd l::[])@ l2
           else l1 @ l2 @ (List.hd l :: [])
else if (croissant (List.hd l) (List.hd (List.tl l)))
     then (tri_pivot croissant l1) @ (List.hd l::[]) @  (tri_pivot croissant l2)
     else (tri_pivot croissant l1) @ (tri_pivot croissant l2 @ (List.hd l::[])) ;;

tri_pivot croissant [4;2;6;1;3;9;5] ;;	      


























































































