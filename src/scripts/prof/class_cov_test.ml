(*
  corebuild -package bigarray,dsfo,lacaml,core_bench -I src/scripts/prof class_cov_test.native
*)

(* micro-benchmarks to figure out a good way to compute
   scatter matrices, _per_ class. *)
open Lacaml.D
open Core_bench.Std

let m_train = Mnist.data `Train |> Mat.transpose_copy
let feature_size = 28 * 28
let x = lacpy ~n:feature_size m_train ;;

let column_to_label c =
  let rec loop i =
    if c.{i + feature_size} = 1.0 then i - 1
    else loop (i + 1) in
  loop 1

let labels = Array.init 60000 (fun r -> column_to_label (Mat.copy_row m_train (r + 1)))

type 'a class_mask =
  { order : 'a list
  ; masks : Vec.t list
  ; sizes : float list
  }

let class_masks cls =
  let rows = List.length cls in
  let htbl = Hashtbl.create (rows / 10) in
  List.iteri (fun i c ->
      match Hashtbl.find htbl c with
      | v -> v.{i + 1} <- 1.0
      | exception Not_found ->
          let v = Vec.make0 rows in
          v.{i + 1} <- 1.0;
          Hashtbl.add htbl c v) cls;
  let order, masks, sizes =
    Hashtbl.fold (fun c v (c_a,v_a,n_a) ->
      let n = Vec.sum v in
      (c::c_a,v::v_a,n::n_a)) htbl ([],[],[])
  in
  {order; masks; sizes}

let class_means_pip data m f =
  let cc = Mat.of_diag (Vec.reci (Vec.of_list m.sizes)) in
  let mm = Mat.of_col_vecs_list m.masks in
  let mn = gemm ~transa:`T data (gemm mm cc) in
  f cc mm mn

let class_means data m =
  class_means_pip data m (fun _ _ mn -> mn)

let index_masks cls =
  let rows = List.length cls in
  let htbl = Hashtbl.create (rows / 10) in
  List.iteri (fun i c ->
      let j = i + 1 in
      match Hashtbl.find htbl c with
      | l -> Hashtbl.replace htbl c (j :: l)
      | exception Not_found ->
          Hashtbl.add htbl c (j::[])) cls;
  Hashtbl.fold (fun c v acc -> (c, v) :: acc) htbl []

let column_means a =
  let n = Mat.dim1 a in
  gemv ~alpha:(1./. (float n)) ~trans:`T a (Vec.make n 1.)

let row_means a =
  let m = Mat.dim2 a in
  gemv ~alpha:(1./.(float m)) a (Vec.make m 1.)

let column_scatter a =
  let n = float (Mat.dim1 a) in
  let u = column_means a in
  let ralpha = -1. in
  let malpha = 1. /. n in
  ger ~alpha:ralpha u u (gemm ~alpha:malpha ~transa:`T a a)

let row_scatter a =
  let m = float (Mat.dim2 a) in
  let u = row_means a in
  let ralpha = -1. in
  let malpha = 1. /. m in
  ger ~alpha:ralpha u u (gemm ~alpha:malpha ~transb:`T a a)

(* Test THIS *)
let class_cov_via_submatrices data cls =
  let dt = Mat.transpose_copy data in
  index_masks cls
  |> List.map (fun (c, idxs) ->
      c,
      Array.of_list idxs
      |> Array.map (Mat.col dt)
      |> Mat.of_col_vecs
      |> row_scatter)

(* vs THIS *)
let class_cov_via_submatrices2 data cls =
  let dt = Mat.transpose_copy data in
  index_masks cls
  |> List.map (fun (c, idxs) ->
      c,
      List.rev idxs (* would sequentially ordering improve perf? ... eh *)
      |> Array.of_list
      |> Array.map (Mat.col dt)
      |> Mat.of_col_vecs
      |> row_scatter)

(* vs THIS, which could be worse because it separates the copying,
   but has almost identical performance. *)
let class_cov_via_submatrices3 data cls =
  index_masks cls
  |> List.map (fun (c, idxs) ->
      c,
      Array.of_list idxs
      |> Array.map (Mat.copy_row data)
      |> Mat.of_col_vecs
      |> row_scatter)

let centered_class a m =
  let mm = Mat.of_col_vecs_list m.masks in
  let mu = class_means a m in
  Mat.sub a (gemm mm ~transb:`T mu)

(* vs THIS, which turns into a disaster, at large values, since we're
  effectively creating (zero'd out) copies when we make 'p'...
  impractical except for very small cases.*)
let class_cov_via_2gemmm data cls =
  let msk = class_masks cls in
  let cnt = centered_class data msk in
  let n = Mat.dim1 data in
  let m = Mat.dim2 data in
  let wsp = Mat.make0 m n in
  msk.masks |> List.mapi (fun i u ->
    let c = List.nth msk.order i in
    let n = List.nth msk.sizes i in
    (* Zero out the non-class members *)
    let p = gemm ~transa:`T cnt (Mat.of_diag u) ~c:wsp ~beta:0. in
    c, gemm ~alpha:(1./.n) ~transb:`T p p)

let find_index ?(start=0) f a =
  let n = Array.length a in
  let rec loop i =
    if i >= n then
      raise Not_found
    else if f a.(i) then
      i
    else
      loop (i + 1)
  in
  loop start

(* vs THIS *)
let class_cov_via_perm data cls =
  let n = Mat.dim1 data in
  let icls = Array.make n (List.hd cls, 0l) in
  List.iteri (fun i c -> icls.(i) <- (c,Int32.of_int (i+1))) cls;
  Array.sort compare icls;
  let p = Lacaml_common.create_int32_vec n in
  for i = 1 to n do p.{i} <- snd icls.(i-1) done;
  let dt = Mat.transpose_copy data in
  lapmt dt p; (* reorder *)
  let rec loop start acc =  (* construct sub matrices *)
    let cc = fst icls.(start) in
    try
      let n = find_index ~start (fun (c, _) -> c <> cc) icls in
      (* if we could perform cov in place, no need for this lacpy *)
      let s = lacpy ~n:(n-start) ~ac:(start+1) dt in
      loop n ((cc, row_scatter s) :: acc)
    with Not_found ->
      let s = lacpy ~ac:(start+1) dt in
      (cc, row_scatter s) :: acc
  in
  loop 0 []

(*
let swap s data =
  for i = 0 to Array.length s - 1 / 2 do
    let t = data.(i) in
    let j = s.(i) in
    data.(i) <- data.(j);
    data.(j) <- t
  done;
  data

let vis m c =
  Mat.col m c
  |> genarray_of_array1
  |> (fun g -> reshape_2 g 28 28)
  |> genarray_of_array2
  |> (fun g -> GA.init Int8_unsigned Fortran_layout [|28; 28|]
         (fun i -> truncate (255. *. GA.get g i)))
*)

(* So far the naive approach is best *)
let () =
  let open Printf in
  if not (!Sys.interactive) then
    let tests_per_sample_size m =
      let x = lacpy ~m x in
      let labels = Array.to_list (Array.sub labels 0 m) in
      [ Bench.Test.create ~name:(sprintf "Using sub matrices: %d" m)
          (fun () -> ignore(class_cov_via_submatrices x labels))
        ; Bench.Test.create ~name:(sprintf "Using sub matrices2: %d" m)
          (fun () -> ignore(class_cov_via_submatrices2 x labels))
        ; Bench.Test.create ~name:(sprintf "Using sub matrices 3: %d" m)
          (fun () -> ignore(class_cov_via_submatrices3 x labels))
        (*; Bench.Test.create ~name:"Using 2 gemms"
          (fun () -> ignore (class_cov_via_2gemmm x labels)) *)
        ; Bench.Test.create ~name:(sprintf "Using permutation: %d" m)
          (fun () -> ignore (class_cov_via_perm x labels))
      ]
    in
    [ 100; 500; 1000; 5000; 10000; 50000]
    |> List.map tests_per_sample_size
    |> List.concat
    |> Bench.make_command
    |> Core.Command.run

