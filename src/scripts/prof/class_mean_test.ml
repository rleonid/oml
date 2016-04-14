(*
  corebuild -package bigarray,dsfo,lacaml,core_bench -I src/scripts/prof class_mean_test.native
*)

open Lacaml.D
open Core_bench.Std

let m_train = Mnist.data `Train |> Mat.transpose_copy
let feature_size = 28 * 28
let x = lacpy ~n:feature_size m_train ;;

let column_to_label c =
  let rec loop i =
    if c.{i + feature_size} = 1.0 then i
    else loop (i + 1) in
  loop 1

let labels = Array.init 60000 (fun r -> column_to_label (Mat.copy_row m_train (r + 1)))

(** TODO: We need a way to write these tests without replicating all of these
    small helper functions. Something ala kaputt that allows you to write code
    inside of the same structure *)
let col_mean n c = Vec.sum c /. n
let column_means a =
  let n = Mat.dim1 a in
  gemv ~alpha:(1./. (float n)) ~trans:`T a (Vec.make n 1.)

(* Test THIS  *)
let class_mean_maps data cls =
  let cols = Mat.dim2 data in
  let rows = Mat.dim1 data in
  let htbl = Hashtbl.create (rows / 2) in
  List.iteri (fun i c ->
    match Hashtbl.find htbl c with
    | l -> Hashtbl.replace htbl c (i :: l)
    | exception Not_found ->
      Hashtbl.add htbl c [i]) cls;
  Hashtbl.fold (fun c idxs a ->
      let v = Array.of_list idxs in
      let rows = Array.length v in
      let cms =
        column_means
          (Mat.init_rows rows cols (fun r c -> data.{v.(r-1)+1, c}))
      in
      (c, cms) :: a) htbl []

let msk_mean mask n c = (dot mask c) /. n

let msk_mean_mat mask n a =
  let m = Mat.dim2 a in
  Vec.init m (fun i -> msk_mean mask n (Mat.col a i))

let class_masks ?(scale=false) cls =
  let rows = List.length cls in
  let htbl = Hashtbl.create (rows / 10) in
  List.iteri (fun i c ->
      match Hashtbl.find htbl c with
      | v -> v.{i + 1} <- 1.
      | exception Not_found ->
          let v = Vec.make0 rows in
          v.{i + 1} <- 1.;
          Hashtbl.add htbl c v) cls;
  Hashtbl.fold (fun c v a ->
      let n = Vec.sum v in
      if scale then scal (1./.n) v;
      (c, (v, Vec.sum v)) :: a) htbl []

let class_means data masks =
  List.map (fun (c, (v, n)) -> 
    c, msk_mean_mat v n data) masks

(* vs THIS *)
let class_mean_masks data labels =
  class_masks labels
  |> class_means data

(* and THIS *)
let class_mean_masks_mm_unsc data labels =
  class_masks labels
  |> (fun lst -> 
      let lst = List.sort compare lst in
      let mm = Mat.of_col_vecs_list (List.map (fun (_, (m, _)) -> m) lst) in 
      let sc = Vec.of_list (List.map (fun (_, (_, n)) -> 1./.n) lst) in
      mm, sc)
  |> (fun (mm, sc) -> gemm ~transa:`T data (gemm mm (Mat.of_diag sc)))

(* and THIS *)
let class_mean_masks_mm data labels =
  class_masks ~scale:true labels
  |> List.sort compare
  |> List.map (fun (_, (m, _)) -> m) 
  |> Mat.of_col_vecs_list 
  |> gemm ~transa:`T data

let () =
  let x, labels =
    if true then
      let m = 30000 in
      lacpy ~m x, (Array.to_list (Array.sub labels 0 m))
    else
      x, Array.to_list labels
  in
  Core.Command.run (Bench.make_command
    [ Bench.Test.create ~name:"Using maps"
      (fun () -> ignore(class_mean_maps x labels))
    ; Bench.Test.create ~name:"Using masks"
      (fun () -> ignore (class_mean_masks x labels))
    ; Bench.Test.create ~name:"Using masks_mm_unsc"
      (fun () -> ignore (class_mean_masks_mm_unsc x labels))
    ; Bench.Test.create ~name:"Using masks_mm"
      (fun () -> ignore (class_mean_masks_mm x labels))
    ])
