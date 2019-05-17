(*Chargement des modules*)
#load "hasard.cmo" ;;
open Hasard ;;
#load "list_tri.cmo" ;;
open List_tri ;;

let l = random_list 100 100 ;;
let a = (>) ;;
tri a l ;;
suppr_doublons l ;;
min_list (<) l ;;