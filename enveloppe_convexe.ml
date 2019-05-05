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

let lp = [{abs = 1 ; ord = 2};{abs =3 ; ord = 4};{abs = 8 ; ord = 6};{abs = 4 ; ord = 9}];;
points_depart lp;;
