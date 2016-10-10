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

let normal_kl_divergence ~p_mean ~p_sigma ~q_mean ~q_sigma =
  let p_sigma_degen =
    not @@ Oml_util.significantly_different_from p_sigma 0.0
  in
  let q_sigma_degen =
    not @@ Oml_util.significantly_different_from q_sigma 0.0
  in
  if p_sigma_degen || q_sigma_degen then infinity else
  let mean_diff = p_mean -. q_mean in
  let mean_diff_sq = mean_diff *. mean_diff in
  let p_sigma_sq = p_sigma *. p_sigma in
  let q_sigma_sq = q_sigma *. q_sigma in
  log (q_sigma /. p_sigma) +. (p_sigma_sq +. mean_diff_sq) /.
      (2.0 *. q_sigma_sq) -. 0.5
