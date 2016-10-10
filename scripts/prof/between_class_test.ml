(*
  corebuild -package lacaml,core_bench -I src/scripts/prof between_class_test.native
*)

open Lacaml.D
open Core_bench.Std

type 'a class_mask =
  { order : 'a list
  ; masks : Vec.t list
  ; sizes : float list
  }

let column_means a =
  let n = Mat.dim1 a in
  gemv ~alpha:(1./. (float n)) ~trans:`T a (Vec.make n 1.)

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
      (c::c_a,v::v_a,n::n_a))
       htbl ([],[],[])
  in
  {order; masks; sizes}

(* Test THIS *)
let class_means data m =
  let cc = Mat.of_diag (Vec.reci (Vec.of_list m.sizes)) in
  let mm = Mat.of_col_vecs_list m.masks in
  gemm ~transa:`T data (gemm mm cc)

(* vs THIS *)
let between_class_ger a m =
  let cls = Mat.dim2 a in
  let cu = column_means a in
  let cm = class_means a m in
  let r = Mat.make0 cls cls in
  let s = Array.of_list m.sizes in
  for i = 1 to (Array.length s) do
    let d = Vec.sub (Mat.col cm i) cu in
    let alpha = s.(i-1) in
    ignore (ger ~alpha d d r);
  done;
  r

let between_class_gemm a m =
  let cu = column_means a in
  let cm = class_means a m in
  let v = Vec.sqrt (Vec.of_list m.sizes) in
  let d = Mat.sub cm (Mat.of_col_vecs_list (List.map (fun _ -> cu) m.sizes)) in
  Mat.scal_cols d v;
  gemm d ~transb:`T d

(* MM is a clear winner by less allocations, but there are sweet spots where ger
   can be 2x faster.  *)
let () =
  let mr n = Mat.random n n in
  let mkc n =
    Array.init n (fun i -> i mod 20)
    |> Array.to_list 
    |> class_masks
  in
  let tens = mr 10 in
  let tenc = mkc 10 in
  let huns = mr 100 in
  let hunc = mkc 100 in
  let huns5 = mr 500 in
  let hunc5 = mkc 500 in
  let thou = mr 1000 in
  let thoc = mkc 1000 in
  let teth = mr 10000 in
  let tetc = mkc 10000 in
  Core.Command.run (Bench.make_command
    [ Bench.Test.create ~name:"Using between_class_ger 10"
        (fun () -> ignore(between_class_ger tens tenc))
    ; Bench.Test.create ~name:"Using between_class_gemm 10"
        (fun () -> ignore(between_class_gemm tens tenc))
    ; Bench.Test.create ~name:"Using between_class_ger 100"
        (fun () -> ignore(between_class_ger huns hunc))
    ; Bench.Test.create ~name:"Using between_class_gemm 100"
        (fun () -> ignore(between_class_gemm huns hunc))
    ; Bench.Test.create ~name:"Using between_class_ger 500"
        (fun () -> ignore(between_class_ger huns5 hunc5))
    ; Bench.Test.create ~name:"Using between_class_gemm 500"
        (fun () -> ignore(between_class_gemm huns5 hunc5))
    ; Bench.Test.create ~name:"Using between_class_ger 1000"
        (fun () -> ignore(between_class_ger thou thoc))
    ; Bench.Test.create ~name:"Using between_class_gemm 1000"
        (fun () -> ignore(between_class_gemm thou thoc))
    ; Bench.Test.create ~name:"Using between_class_ger 10000"
        (fun () -> ignore(between_class_ger teth tetc))
    ; Bench.Test.create ~name:"Using between_class_gemm 10000"
        (fun () -> ignore(between_class_gemm teth tetc))
    ])
 
