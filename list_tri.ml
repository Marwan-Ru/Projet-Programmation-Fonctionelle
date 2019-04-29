(* module list tri *)

(* Tri partition-fusion *)

(* 1. *)
(*
let rec partitionne_bis l rang l1 l2 =
match l with
[] -> (l1,l2)
|x::r -> if rang mod 2 == 0 
         then partitionne_bis r (rang+1) (x::l1) l2
         else partitionne_bis r (rang+1) l1 (x::l2) ;;

let partitionne l = 
match l with
[] -> ([],[])
|x::r -> partitionne_bis l 0 [] [] ;;

(* 2. *)

let rec fusionne_bis comp lc =
match lc with
([], []) -> []
|([],a::b) -> a::b
|(x::r,[]) -> x::r
|(x::r, a::b) -> if comp x a then x::(fusionne_bis comp (r, a::b))
                 else a::(fusionne_bis comp (x::r,b)) ;;

let fusionne comp l1 l2 = fusionne_bis comp (l1,l2) ;;

(* 3. *)
let rec tri_partition_fusion comp l =
if l = []
then []
else let (l1, l2) = partitionne l in
     if l1 = [] && l2 =[]
     then []
     else if (List.length l1 <= 1) && (List.length l2 <= 1)
          then fusionne comp l1 l2
          else fusionne comp (tri_partition_fusion comp l1) (tri_partition_fusion comp l2) ;;
*)

(* Tri pivot *)

(* 1. *)

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

(* 2. *)

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

(*
(* Tri à bulle *)

(* 1. *)

let rec tri_bulle_bis comp l =
match l with
[] -> []
| x::[] -> l
| x::y::r -> if (comp x y) 
             then x::(tri_bulle_bis comp (y::r))
             else y::(tri_bulle_bis comp (x::r)) ;;

(* 2. *)

let rec tri_bulle comp l =
if l = (tri_bulle_bis comp l)
then l
else (tri_bulle comp (tri_bulle_bis comp l)) ;;
*)

let tri comp l = tri_pivot comp l ;;


(*Min_list*)


let rec min_list_bis comp l x =
match l with
[] -> x
|a::r -> if comp x a
         then min_list_bis comp r x
         else min_list_bis comp r a ;;
	 
let min_list comp l =
match l with
[] -> failwith "Liste vide"
|a::r -> min_list_bis comp r a ;;

(* Suppr_doublons *)

let rec suppr_doublons l = 
let ltrie = tri_pivot (<) l in
match ltrie with 
[] -> []
|x::y::r -> if x = y
            then suppr_doublons (x::r)
            else x::(suppr_doublons (y::r))
|x::r -> x::(suppr_doublons r);;

(* test temps *)
(*
let list = tri_pivot (>=) (random_list 1000 1000) ;;

let c = (>=) ;; 

let temps_p_f =
let temps_debut = Sys.time () in
let _ = tri_partition_fusion c list in
let temps_fin = Sys.time () in
(temps_fin -. temps_debut) ;;

let temps_pivot =
let temps_debut = Sys.time () in
let _ = tri_pivot c list in
let temps_fin = Sys.time () in
(temps_fin -. temps_debut) ;;

let temps_bulle =
let temps_debut = Sys.time () in
let _ = tri_bulle c list in
let temps_fin = Sys.time () in
(temps_fin -. temps_debut) ;; 
*)























































































