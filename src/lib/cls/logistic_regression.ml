(*
   Copyright 2015,2016:
     Leonid Rozenberg <leonidr@gmail.com>

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
*)

open Util
open Cls_intf

module LrCommon(Data: Continuous_encoded_data) = struct

  open Lacaml.D

  type feature = Data.feature
  type clas = Data.clas

  type samples = (clas * feature) list

  type opt =
    { lambda    : float
    ; tolerance : float
    }

  let opt ?(lambda=1e-4) ?(tolerance=1e4) () =
    { lambda ; tolerance }

  let default = opt ()

  let safe_encoding f =
    let e = Data.encoding f in
    let l = Array.length e in
    if l = Data.size then
      e
    else
      invalid_arg ~m:"Continuous_encoded_data" ~f:"encoding"
        "size %d actual %d" Data.size l

  let copy1 arr = Array.init (Data.size + 1) (function | 0 -> 1. | i -> arr.(i - 1))

  (* map classes to [1;2 ... 3], convert features to matrix and run Softmax *)
  let estimate ~m ~class_bound ~to_t ?(opt=default) ?(classes=[]) data =
    let class_bound =
      match class_bound with
      | None   -> fun n -> n
      | Some b -> min b
    in
    if data = [] then
      invalid_arg ~m ~f:"estimate" "Nothing to train on!" 
    else
      let error_on_new = classes <> [] in
      let assigned_cls_assoc =
        ref (List.mapi ~f:(fun i c -> c, class_bound (i + 1)) classes)
      in
      let classes =
        List.mapi data ~f:(fun idx (c, _) ->
          try
            List.assoc c !assigned_cls_assoc
          with Not_found ->
            if error_on_new then
              invalid_arg ~m ~f:"estimate"
                "Found a new (unexpected) class at datum %d" idx
            else
              let n = class_bound (List.length !assigned_cls_assoc + 1) in
              assigned_cls_assoc := (c, n) :: !assigned_cls_assoc;
              n)
        |> Array.of_list
      in
      if List.length !assigned_cls_assoc < 2 then
        invalid_arg ~m ~f:"estimate"
          "Trying to estimate Log Reg on less than 2 classes."
      else
        let ftrs =
          List.map ~f:(fun (_, f) -> copy1 (safe_encoding f)) data
          |> Array.of_list
          |> Mat.of_array
        in
        let weights =
          Softmax_regression.regress
            ~lambda:opt.lambda
            ~tolerance:opt.tolerance
            ftrs classes
        in
        let sortedc =
          List.sort ~cmp:(fun (_, n1) (_, n2) -> compare n1 n2)
            !assigned_cls_assoc
          |> List.map ~f:fst
        in
        to_t weights sortedc

end

module Binary(Data: Continuous_encoded_data) = struct

  include LrCommon(Data)
  open Lacaml.D

  type t =
    { weights    : vec
    ; classes    : clas list
    }

  let coefficients t = Vec.to_array t.weights

  let base_class t = List.hd t.classes

  let proba w x = Float.(1. / (1. + exp (dot w x)))

  let eval lr feature =
    let a = safe_encoding feature in
    let m = Vec.of_array (copy1 a) in
    let p = proba lr.weights m in
    (base_class lr, p) ::
      (List.map ~f:(fun c -> c, (1. -. p)) (List.tl lr.classes))

  let estimate = estimate
      ~m:"Binary"
      ~class_bound:(Some 2)
      ~to_t:(fun weights classes ->
              let r1 = Mat.copy_row weights 1 in
              let r2 = Mat.copy_row weights 2 in
              { weights = Vec.sub r2 r1; classes})

end

module Multiclass(Data: Continuous_encoded_data) = struct

  include LrCommon(Data)
  open Lacaml_D

  type t =
    { weights : mat
    ; classes : clas list
    }

  let coefficients t = Mat.to_array t.weights

  let class_order t = t.classes

  let eval lr feature =
    let a   = safe_encoding feature in
    let x_i = Vec.of_array (copy1 a) in
    let prs = Softmax_regression.classify_v lr.weights x_i in
    List.map2 ~f:(fun c (_, p) -> (c, p)) lr.classes prs

  let estimate = estimate
      ~m:"Multiclass"
      ~class_bound:None
      ~to_t:(fun weights classes -> { weights ; classes})

end
