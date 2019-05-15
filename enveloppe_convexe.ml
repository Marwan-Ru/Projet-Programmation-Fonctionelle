#load "graphics.cma";;
#load "point.cmo";;
open Point;;
#load "list_tri.cmo";;
open List_tri;;
open Graphics;;

let min_point (p1 : point) (p2 : point) =
if p1 < p2
then true
else false ;;

let max_point (p1 : point) (p2 : point) =
if p1 > p2
then true
else false ;;

let points_depart (l : point list) =
(min_list (min_point) l, min_list (max_point) l) ;;

let lp = [{abs = 1 ; ord = 2};{abs = -3 ; ord = 4};{abs = 8 ; ord = 6};{abs = -4 ; ord = 9};{abs = 9 ; ord = 9};{abs = 5; ord = -4};{abs = 6 ; ord = 3}];;
points_depart lp;;
let p2 = {abs = 0 ; ord = 0} ;;
let p1 = {abs = -3 ; ord = 4} ;;
let pa = {abs = -1 ; ord = 3};;
let pb = {abs = 2 ; ord = 5} ;;

let rec point_droite (l : point list) (p1 : point) (p2 : point) =
let x1 = p1.abs in
let y1 = p1.ord in
let x2 = p2.abs in
let y2 = p2.ord in
match l with
[] -> []
|x::r -> if ((x2-x1)*((x.ord)-y1)-(y2-y1)*((x.abs)-x1)) < 0
         then x::(point_droite r p1 p2) 
         else (point_droite r p1 p2) ;; 

let equation_droite (p1 :point) (p2 : point) = 
let x1 = p1.abs in
let y1 = p1.ord in
let x2 = p2.abs in
let y2 = p2.ord in
( (y2-y1) ,-(x2-x1) ,-((y2-y1)*x1+(-(x2-x1))*y1)) ;;

let distance_droite a b c (p:point) = 
let x = p.abs in
let y = p.ord in
(float_of_int(abs( a * x + b * y + c))/.sqrt(float_of_int(a*a + b*b))) ;;

let distance_max p1 p2 pa pb = 
let (a,b,c)=equation_droite p1 p2 in
let dpa = distance_droite a b c pa in 
let dpb = distance_droite a b c pb in 
if dpa > dpb 
then true 
else false ;;

let rec distance_list (p1 : point) (p2 : point) (l : point list) =
let (a,b,c) = equation_droite p1 p2 in
match l with 
[] -> []
|x::r -> (distance_droite a b c x)::(distance_list p1 p2 r);;

let rec point_eloigne (p1 : point) (p2:point) (l : point list) =
let (a,b,c) = equation_droite p1 p2 in 
let distance_list = distance_list p1 p2 l in
let test = min_list (>) distance_list in
match l with
[] -> failwith "Erreur, la liste est vide"
|x::r -> if (distance_droite a b c x) = test
         then x 
         else point_eloigne p1 p2 r ;; 

let points_egaux (p1 : point) (p2: point) =
let x1 = p1.abs in
let x2 = p2.abs in
let y1 = p1.ord in
let y2 = p2.ord in
if x1 == x2 && y1 == y2
then true
else false ;;

let rec ajoute_list_apres (p1: point) (p: point) (l: point list)=
match l with
[] -> [p]
|x::r -> if points_egaux x p1 then x::p::r
         else x::(ajoute_list_apres p1 p r) ;;

let rec findhull l p q enveloppe =
match l with
[] -> enveloppe
| x::r -> let c = point_eloigne p q l in
let enveloppe = ajoute_list_apres p c enveloppe in
let enveloppe = findhull (point_droite l p c) p c enveloppe in
findhull (point_droite l c q) c q enveloppe;;

let quickhull l =
let l = suppr_doublons l in
let (pa,pb) = points_depart l in
let enveloppe = pa::(pb::[]) in
let enveloppe = findhull (point_droite l pa pb) pa pb enveloppe in
findhull (point_droite l pb pa) pb pa enveloppe;;

let enveloppe_convexe g n =
let l = g n in
(vider ();
set_color red;
tracer_nuage l ;
set_color blue;
tracer_polygone(quickhull l));;

initialiser();;
enveloppe_convexe gen_soleil 2000 ;;




























































































































