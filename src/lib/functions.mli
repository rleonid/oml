(*
   Copyright 2015:
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

(** Implementations of basic functions needed to compute distributions.

  At the present moment many functions are wrappers to the Cephes library
  (http://www.netlib.org/cephes/), via a Ctypes interface implemented in
  the {{:https://github.com/rleonid/ocephes}Ocephes} library.
 *)

(** [erf x] computes the (Gauss) error function,
  (2/sqrt(pi) * \int_0^x e^-t^2 dt) *)
val erf : float -> float

(** [erfc x] computes [1.0 - erfc x].*)
val erfc : float -> float

(** [gamma x] computes the gamma function of [x]. For positive integers
    [gamma x] approximates [(x - 1)!]. *)
val gamma : float -> float

(** [ln_gamma x] compute the natural logarithm of the gamma function of [x].

    It is usually more accurate to use [ln_gamma] instead of [gamma] and
    afterwards compute the exponent.*)
val ln_gamma : float -> float

(** [regularized_lower_gamma a x] computes regularized (normalized by
    [gamma a]) incomplete lower (integral from 0 to [x]) function. *)
val regularized_lower_gamma : float -> float -> float

(** [regularized_upper_gamma a x] computes regularized (normalized by
    [gamma a]) incomplete upper (integral from [x] to [infinity]) function. *)
val regularized_upper_gamma : float -> float -> float

(** [beta x y] computes the beta function of [x] and [y], this function is also
    known as the Euler integral of the first kind and is useful in defining
    various distributions. *)
val beta : float -> float -> float

(** [ln_beta x y]  computes [log (beta x y)], for more accuracy.*)
val ln_beta : float -> float -> float

(** [regularized_beta ?epsilon ?max_iterations ~alpha ~beta x] computes
    the regularized (divided by [beta alpha beta]) incomplete beta function,
    which is the partial (0 to [x]) integral of the beta function paramterized
    by [alpha] and [beta]. *)
val regularized_beta : alpha:float -> beta:float -> ?epsilon:float ->
  ?max_iterations:int -> float -> float

(** [softmax ?temperature weights] transforms [weights] into softmax weights dependent
    on [temperature].

    @raise Invalid_argument if [weights] is empty or [temperature = 0]. *)
val softmax : ?temperature:float -> float array -> float array

(** {2 Distribution CDF's} *)

(** [chi_square_less k x] computes the probability of
    seeing a value less than [x] in a Chi-square distribution with [k] degrees
    of freedom.*)
val chi_square_less : int -> float -> float

(** [chi_square_greater k x] computes the probability of seeing a value
    greather than [x] in a Chi-square distribution with [k] degrees of
    freedom.*)
val chi_square_greater : int -> float -> float

(*val t_lookup : float -> int -> float *)

(** [softmax ?temperature weights] transforms [weights] into softmax weights dependent
    on [temperature].

    @raise Invalid_argument if [weights] is empty or [temperature = 0]. *)
val softmax : ?temperature:float -> float array -> float array

(** [normal_cdf_inv x] returns the value [y] such that the integral of the
    normal cdf is [x]. *)
val normal_cdf_inv : float -> float

(** [student_cdf_inv k x] returns the value [y] such that the integral of the
    Students T distribution with [k] degrees of freedom is [x].*)
val student_cdf_inv : int -> float -> float

(** [f_less d1 d2 x] computes the probability of seeing a value less than [x]
    in an F-distribution parameterized by [d1] and [d2]. *)
val f_less : d1:float -> d2:float -> float -> float
