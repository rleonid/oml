open Lacaml.D 
module LU = Uncategorized.Lacaml_util
let m = Mat.random 10 3  ;;
let r = [1;2;3;1;2;3;1;2;3;1] ;;

#require "dsfo"  ;;
let i = Iris.iris  ;;
let m = List.map snd i |> Array.of_list |> Mat.of_array  ;;
let c = List.map fst i  ;;
let masks = LU.class_masks c  ;;
let means = LU.class_means m masks ;; 
let centr = LU.centered_class m masks ;;
let witc = LU.within_class_scatter m masks ;;
let btwn = LU.between_class_scatter m masks ;;
