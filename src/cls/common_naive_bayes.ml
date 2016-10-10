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

open Oml_util

let multiply_ref = ref true

(* How we choose to multiply large arrays of small probabilities.
  TODO: develop a smart heuristic for switching to log transformed addition. *)
let prod_arr, prod_arr2 =
  if !multiply_ref then
    (fun f x -> Array.fold_left (fun p x -> p *. f x) 1.0 x),
    (fun f x y -> Array.fold2 (fun p x y -> p *. f x y) 1.0 x y)
  else
    (fun f x -> Array.fold_left (fun s x -> s +. log (f x)) 0.0 x |> exp),
    (fun f x y -> Array.fold2 (fun s x y -> s +. log (f x y)) 0.0 x y |> exp)

let within a b x = max a (min x b)

let smoothing_to_prob = function
  | 0.0    ->
      (fun count bkgrnd _ -> count /. bkgrnd)
  | sf ->
      let sf = within 0.0 1.0 sf in
      (fun count bkgrnd space_size ->
        (count +. sf) /. (bkgrnd +. sf *. space_size))

(* likelihood refers to the conditional probability of observing a feature f_i
for class C_k : P(f_i|C_k) *)

(* Simplify the evaluate code by keeping track of the evidence.*)
let eval ~to_prior ~to_likelihood cls_assoc =
  let evidence = ref 0.0 in
  List.map cls_assoc ~f:(fun (c, e) ->
    let prior = to_prior e in
    let likelihood = to_likelihood e in
    let prob = prior *. likelihood in
    evidence := !evidence +. prob;
    (c, prob))
  |> List.map ~f:(fun (c, p) -> (c, p /. !evidence))

(* init - init per class data
   update - update per class data
   incorporate - convert class assoc to final shape *)
let estimate m (type c) init update incorporate
  (module Cm : Map.S with type key = c) ?(classes=[]) (data : (c * 'a) list) =
  let ia fmt = invalid_arg ~m ~f:"estimate" fmt in
  if data = [] then
    ia "Nothing to train on"
  else
    let error_on_new = classes <> [] in
    let init_map =
      List.fold_left classes
        ~f:(fun m c -> Cm.add c (init c) m)
        ~init:Cm.empty
    in
    let total, first_pass =
      List.fold_left data
        ~f:(fun (total, m) (cls, ftr) ->
          let m' =
            try
              let a = Cm.find cls m in
              Cm.add cls (update a ftr) m
            with Not_found ->
              if error_on_new then
                (*ia (Printf.sprintf "Unexpected class at datum %d" total*)
                ia "Unexpected class at datum %d" total
              else
                Cm.add cls (update (init cls) ftr) m
          in
          (total + 1, m'))
        ~init:(0, init_map)
    in
    let num_classes = Cm.cardinal first_pass in
    incorporate (Cm.bindings first_pass) num_classes (float total)
