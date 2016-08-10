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

(** {1 Probability density, cumulative and quantile functions common distributions.} *)

(** {2 Normal } *)

(** [normal_cdf mean std x] is the probability that a normal random
    variable with [mean] and standard deviation [std] takes a value less than
    or equal to [x]. [mean] defaults to 0.0 and [std] to 1.0.
*)
val normal_cdf : ?mean:float -> ?std:float -> float -> Probability.t

(** [normal_pdf mean std x] is the value of the Normal distribution
    function (with [mean] and standard deviation [std]) at [x].
    [mean] defaults to 0.0 and [std] to 1.0.
*)
val normal_pdf : ?mean:float -> ?std:float -> float -> float

(** [normal_quantile ?mean ?std p] computes [x] such that
    [normal_cdf mean std x = p]. [mean] defaults to 0.0 and [std] to 1.0. *)
val normal_quantile : ?mean:float -> ?std:float -> Probability.t -> float

(** {2 Poisson} *)

(** [poisson_cdf mean x] is the probability that a Poisson random variable with
    [mean] will take a value less than or equal to [x]. *)
val poisson_cdf : mean:float -> float -> Probability.t

(** {2 Beta} *)
(** [beta_pdf ~alpha ~beta x] natural log value of the Beta distribution
    function (with [alpha] and [beta] shape parameters) at [x].*)
val ln_beta_pdf : alpha:float -> beta:float -> float -> float

(** [beta_pdf ~alpha ~beta x] value of the Beta distribution
    function (with [alpha] and [beta] shape parameters) at [x].*)
val beta_pdf : alpha:float -> beta:float -> float -> float

(** [beta_cdf ~alpha ~beta x] probability that a beta random variable with
    [alpha] and [beta] shape parameters takes a value less than or equal to
    [x]. *)
val beta_cdf : alpha:float -> beta:float -> float -> Probability.t

(** {2 Chi} *)

(** [chi_square_cdf ~k x] computes the probability of seeing a value less than
    [x] in a Chi-square distribution with [k] degrees of freedom.*)
val chi_square_cdf : k:int -> float -> Probability.t

(** {2 Student} *)

(** [student_pdf degrees_of_freedom x] computes the value of the Student T's
    distribution with [degrees_of_freedom] at [x].*)
val student_pdf : degrees_of_freedom:int -> float -> float

(** [student_cdf degrees_of_freedom x] computest the probability that the Student T's
    distrubtion with [degrees_of_freedom] takes a value less than [x].*)
val student_cdf : degrees_of_freedom:int -> float -> Probability.t

(** [student_quantile degrees_of_freedom p] computes [x] such that
    [student_cdf degrees_of_freedom x = p]. *)
val student_quantile : degrees_of_freedom:int -> Probability.t -> float

(** {2 Dirichlet} *)

(** [ln_dirichlet_pdf] *)
val ln_dirichlet_pdf : alphas:(float array) -> Probability.s -> float

(** [dirichlet_pdf] *)
val dirichlet_pdf : alphas:(float array) -> Probability.s -> float

(** {2 Benford} *)

(** [benford_pdf base digits] computes the probability of the
  sequence of [digits] in the given [base] (10 by default) as determined
  by Benford's Law.

  @raise Invalid_argument if [base] <= 1, [base] > 10 or [digits] < 0. *)
val benford_pdf : ?base:int -> digits:int -> float

(** [benford_pdf_int64 base digits] computes the probability of the
  sequence of [digits] in the given [base] (10 by default) as determined
  by Benford's Law.

  @raise Invalid_argument if [base] <= 1, [base] > 10 or [digits] < 0. *)
val benford_pdf_int64 : ?base:int -> digits:Int64.t -> float

(** [benford_pdf_seq base digits] computes the probability of the
  sequence of [digits] in the given [base] (10 by default) as determined
  by Benford's Law.

  @raise Invalid_argument if [base] <= 1, empty [digits],
  or any digit < 0 or >= [base] *)
val benford_pdf_seq : ?base:int -> int list -> float
