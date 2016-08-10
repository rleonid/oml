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

(** Infer probabilities from data and perform hypothesis tests. *)

(** [prediction_interval stats alpha]
  Creates a prediction interval for the distribution described by [stats]
  at an [alpha] level of statistical significance; future observations
  will fall within the bounds with probability [1.0 - alpha].

  When we do not know the mean or standard deviation of a distribution
  we can still create a prediction interval based off of basic sampled
  statistics and Student's distribution. *)
val prediction_interval : Descriptive.summary -> float -> float * float

(** A hypothesis test.  *)
type t =
  { degrees_of_freedom : float  (** Can be non-integer due to corrections. *)
  ; statistic          : float  (** The value that we're testing. *)
  ; standard_error     : float  (** The scaled version of the statistic. *)
  ; prob_by_chance     : Probability.t
                                (** The probability that statistic could be
                                    this large (or larger) by chance, for the
                                    specified conditions of the test. *)
  }

(** Describe a hypothesis test. *)
val test_to_string : t -> string

(** [chi observed expected] computes Pearson's Chi squared test of drawing
  [observed] data from the the same categorical distribution as [expected]. *)
val chi : float array -> float array -> t

type null_hypothesis =
  | Two_sided   (** the sample mean equals the population mean. *)
  | One_sided   (** the sample mean is less than or greater than
                    the population mean. *)

(** [t_test nh k d e] conducts a simple T test, against a [nh] null
    hypothesis, where [d] is the difference between population parameter and
    the observed value, [e] is the standard error of the observed value, and
    [k] is the degrees of freedom in the statistical procedure.

    One may think of this as a principled way to test the signal (diff)
    to noise (error) seen in a sample of data. *)
val t_test : null_hypothesis -> degrees_of_freedom:int -> diff:float
              -> error:float -> t

(** [mean_t_test population_mean nh samples] conduct a T-test to see if the
    [sample]'s mean is different from the [population_mean] according to the
    null hypothesis [nh].  *)
val mean_t_test : float -> null_hypothesis -> float array -> t

(** [means_same_variance_test nh sample1 sample2] if we can assume that
    [sample1] and [sample2] have the same variance, test whether they have
    the same mean given the null hypothesis [nh]. *)
val means_same_variance_test : null_hypothesis -> float array
                                      -> float array -> t

(** [means_different_variance_test nh sample1 sample2] when we cannot assume
    that [sample1] and [sample2] have the same variance, test whether they
    do indeed have the same mean given the null hypothesis [nh]. AKA Welch's
    test.  *)
val means_different_variance_test : null_hypothesis -> float array
                                      -> float array -> t

(** [variance_ratio_test sample1 sample2] tests the data in [sample1] and
    [sample2] have the same variance based on F-test.*)
val variance_ratio_test : float array -> float array -> t

