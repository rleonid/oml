
open Lacaml.D
open Util

let check_derivative_numerically ~newmethod x y =
  let k = Array.fold_left max 1 y in
  let gre = Softmax_regression.general_eval_and_grad ~newmethod ~lambda:0.0 k x y in
  let n = Mat.dim2 x in
  let s = n * k in
  (fun ?(d=epsilon_float) () ->
    let w = Vec.random s in
    let g = Vec.make0 s in
    (* insert the desired derivative into g *)
    ignore (gre w g);
    let wc = copy w in
    let nd i = Estimations.second_order (fun x -> wc.{i} <- x; gre wc g) in
    let da = Array.init s (fun i -> let i = i + 1 in nd i w.{i}, g.{i}) in
    Array.all (fun (d1,d2) -> equal_floats ~d d1 d2) da, da)


let x =
  Data.Iris.iris
  |> List.map (fun (_, a) -> [| 1.; a.(0); a.(1); a.(2); a.(3)|])
  |> Array.of_list
  |> Mat.of_array

let y =
  Data.Iris.iris
  |> List.map fst
  |> List.map (function | `setosa -> 1 | `versicolor -> 2 | `virginica -> 3)
  |> Array.of_list 


let score p y =
  Array.fold_left (fun (s, j) i ->
    let v = (Mat.col p j).{i} in
    s +. log v, j + 1)
  (0.,1) y
  |> fst
 

let gren = Softmax_regression.general_eval_and_grad ~newmethod:true ~lambda:0. 3 x y
let greo = Softmax_regression.general_eval_and_grad ~newmethod:false ~lambda:0. 3 x y

let test_value n greo gren =
  (fun ?(d=epsilon_float) () ->
     let w = Vec.random n in
     let g = Vec.make0 n in
     let v1 = greo w g in
     let v2 = gren w g in
     Util.equal_floats ~d v1 v2, v1, v2)

let test_derive n greo gren =
  (fun ?(d=epsilon_float) () ->
    let w = Vec.random n in
    let g = Vec.make0 n in
    ignore (greo w g);
    let gc = copy g in
    scal 0. g;
    ignore (gren w g);
    Vec.ssqr_diff gc g < d, gc, g)

