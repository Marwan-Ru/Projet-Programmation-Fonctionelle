#load "graphics.cma";;
#load "point.cmo";;
open Point;;
#load "list_tri.cmo";;
open List_tri;;
open Graphics;;
(*
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
*)

let lp = [{abs = 1 ; ord = 2};{abs =3 ; ord = 4};{abs = 8 ; ord = 6};{abs = 4 ; ord = 9};{abs = 9 ; ord = 9};{abs = 5; ord = 4};{abs = 6 ; ord = 3};];;
points_depart lp;;
let p1 = {abs = 1 ; ord = 2};;
let p2 = {abs = 3 ; ord = 4} ;;

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

point_droite lp p1 p2 ;;

let equation_droite (p1 :point) (p2 : point) = 
let x1 = p1.abs in
let y1 = p1.ord in
let x2 = p2.abs in
let y2 = p2.ord in
( (y2-y1),-(x2-x1),-((y2-y1)*x1+(-(x2-x1))*y1)) ;;

let distance_droite a b c (p:point) = 
let x = p.abs in
let y = p.ord in
(float_of_int(abs( a * x + b * y + c))/.sqrt(float_of_int(a*a + b*b))) ;;

let distance_max p1 p2 pa pb = 
let (a,b,c)=equation_droite p1 p2 in
let dpa = distance_droite a b c pa in 
let dpb = distance_droite a b c pb in 
if dpa < dpb 
then true 
else false ;;


































































































































