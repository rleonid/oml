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


(** Provides various measurement functions from statistics and analysis *)


val normal_kl_divergence : p_mean:float -> p_sigma:float
  -> q_mean:float -> q_sigma:float -> float
(** Computes kl divergence of the normal distributions P and Q defined
    given means [p_mean] and [q_mean] and std deviations [p_sigma]
    and [q_sigma]. Returns infinity for variances near zero. *)
