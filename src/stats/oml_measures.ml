(*
   Copyright 2015:
     Carmelo Piccione <carmelo.piccione@gmail.com>
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

open MoreLabels
module List = ListLabels

let invalid_arg ~f fmt =
  Oml_util.invalid_arg ~m:"Measure" ~f fmt

let normal_kl_divergence ?d ~p_mean ~p_sigma ~q_mean ~q_sigma () =
  let open Oml_util in
  let p_sigma_degen = not @@ significantly_different_from ?d p_sigma 0.0 in
  let q_sigma_degen = not @@ significantly_different_from ?d q_sigma 0.0 in
  if p_sigma_degen || q_sigma_degen then infinity else
  let mean_diff = p_mean -. q_mean in
  let mean_diff_sq = mean_diff *. mean_diff in
  let p_sigma_sq = p_sigma *. p_sigma in
  let q_sigma_sq = q_sigma *. q_sigma in
  log (q_sigma /. p_sigma) +. (p_sigma_sq +. mean_diff_sq) /.
      (2.0 *. q_sigma_sq) -. 0.5

(* TODO: I'm not too fond of the P,Q KL-divergence nomenclature; it doesn't the
   asymmetry between the two definitions.
   Proposals: (P, Q respectively):
     -  true, approximate

  TODO: allow parameterization over different logs, ex. nats vs bits.
*)
let discrete_kl_divergence ?d (type a) ~(p : (a * 'b) list) ~q () =
  let open Printf in
  let open Oml_util in
  let module O = struct type t = a let compare = compare end in
  let module S = Set.Make(O) in
  let module M = Map.Make(O) in
  let invalid_if b s =
    if b then invalid_arg ~f:"discrete_kl_divergence" "%s" s else ()
  in
  let not_within_prob_range p = p < 0. || 1. < p in
  (* TODO: We should probably have different strategies for aggregating keys.
     Consider the case where one PDF is missing a key. I prefer this approach
     since it lets you describe a large distribution over unobserved spaces.

     This logic will be tied to how we evalaute missing keys in the summation
     loop at the end. *)
  let across_distribution_assoc name =
    fun (s, m, a) (key, prob) ->
      invalid_if (M.exists ~f:(fun k _ -> k = key) m)
        (sprintf "Duplicate keys in %s distribution" name);
      invalid_if (not_within_prob_range prob)
        (sprintf "Probability value %f not within range for %s distribtion" prob name);
        S.add key s, M.add ~key ~data:prob m, Kahan.update a prob
  in
  let keys, mp, s =
    List.fold_left p ~init:(S.empty, M.empty, Kahan.empty)
      ~f:(across_distribution_assoc "P")
  in
  let ks = Kahan.sum s in
  invalid_if (significantly_different_from ?d ks 1.)
    (sprintf "P probabilities don't sum to 1: %f" ks);
  let keys, mq, s =
    List.fold_left q ~init:(keys, M.empty, Kahan.empty)
      ~f:(across_distribution_assoc "Q")
  in
  let ks = Kahan.sum s in
  invalid_if (significantly_different_from ?d ks 1.)
    (sprintf "Q probabilities don't sum to 1: %f" ks);
  let k_up_with_dg_c = Kahan.update_with_degenerate_check in
  S.fold keys
    ~init:Kahan.empty (* TODO: is the accuracy warrented? *)
    ~f:(fun key s ->
          match M.find key mp with
          | exception Not_found     -> s
          | 0.                      -> s
          | p  ->
              match M.find key mq with
              | exception Not_found -> k_up_with_dg_c s infinity
              | 0.                  -> k_up_with_dg_c s infinity
              | q                   -> k_up_with_dg_c s (p *. log ( p /. q)))
  |> Kahan.sum
