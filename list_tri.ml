(* module list tri *)

(* Tri pivot *)

(* 1. *)

(*
let rec partitionne_pivot_bis comp l pivot l1 l2 =
match l with
[] -> (l1,l2)
|x::r -> if (comp x pivot)
         then partitionne_pivot_bis comp r pivot (x::l1) l2
         else partitionne_pivot_bis comp r pivot l1 (x::l2) 

let partitionne_pivot comp l pivot =
match l with 
[] -> ([],[]) 
|x::r -> partitionne_pivot_bis comp l pivot [] [] ;;

(*
 let list = random_list 100 20 ;;
 *)

partitionne_pivot (>) [4;2;6;1;1;3;9;5] 6 ;;
*)

(* 2. *)

(*
let rec tri_pivot comp l =
if l = []
then []
else let pivot = (List.hd l) in
     let (l1,l2) = partitionne_pivot comp (List.tl l) pivot in
     if l1 =[] && l2 =[]
     then pivot::[]
     else if l1 = [] && l2 <> [] 
          then ((List.hd l) :: []) @ (tri_pivot comp l2)
          else if l1<>[] && l2 = []
               then (tri_pivot comp l1) @ ((List.hd l) :: [])
               else (tri_pivot comp l1) @ ((List.hd l) :: [])@(tri_pivot comp l2) ;;

let list = random_list 50 20 ;;

tri_pivot (>) list ;;
*)


(* Tri à bulle *)

(* 1. *)

let rec tri_bulle_bis comp l =
match l with
[] -> []
| x::[] -> l
| x::y::r -> if (comp x y) 
             then x::(tri_bulle_bis comp (y::r))
             else y::(tri_bulle_bis comp (x::r)) ;;

tri_bulle_bis (<=) [4;2;6;1;3;9;5] ;;

(* 2. *)

let rec tri_bulle comp l =
if l = (tri_bulle_bis comp l)
then l
else (tri_bulle comp (tri_bulle_bis comp l)) ;;

tri_bulle (<=) [4;2;6;1;3;9;5] ;;


























































































