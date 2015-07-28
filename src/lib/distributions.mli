(** Probability density functions and cumulative distribution functions for
    some commont distributions. *)

(** [normal_cdf mean std x] probability that a normal random
    variable with [mean] and standard deviation [std] takes a value less than
    or equal to [x]. *)
val normal_cdf : mean:float -> std:float -> float -> float

(** [normal_pdf mean std x] value of the Normal distribution
    function (with [mean] and standard deviation [std]) at [x].*)
val normal_pdf : mean:float -> std:float -> float -> float

(** [normal_quantile ~mean ~std p] computes [x] such that
    [normal_cdf mean std x = p].

    @raise Invalid_argument if [p] is outside [0,1].
*)
val normal_quantile : mean:float -> std:float -> float -> float

(** [standard_normal_cdf x] is equivalent to [normal_cdf ~mean:0 ~std:1 x]. *)
val standard_normal_cdf : float -> float

(** [standard_normal_pdf x] is equivalent to [normal_pdf ~mean:0 ~std:1 x]. *)
val standard_normal_pdf : float -> float

(** [poisson_cdf mean x] the probability that a Poisson random variable with
    [mean] will take a value less than or equal to [x]. *)
val poisson_cdf : mean:float -> float -> float

(** [beta_pdf ~alpha ~beta x] natural log value of the Beta distribution
    function (with [alpha] and [beta] shape parameters) at [x].*)
val ln_beta_pdf : alpha:float -> beta:float -> float -> float

(** [beta_pdf ~alpha ~beta x] value of the Beta distribution
    function (with [alpha] and [beta] shape parameters) at [x].*)
val beta_pdf : alpha:float -> beta:float -> float -> float

(** [beta_cdf ~alpha ~beta x] probability that a beta random
    variable with [alpha] and [beta] shape parameters takes a value less than
    or equal to [x]. *)
val beta_cdf : alpha:float -> beta:float -> float -> float

(** [chi_square_cdf ~k x] computes the probability of seeing a value less than
    [x] in a Chi-square distribution with [k] degrees of freedom.*)
val chi_square_cdf : k:int -> float -> float

(** [student_pdf k x] computes the value of the Student T's distribution with
    [k] degrees of freedom at [x].*)
val student_pdf : k:int -> float -> float

(** [student_cdf k x] computest the probability that the Student T's
    distrubtion with [k] degrees of freedom takes a value less than [x].*)
val student_cdf : k:int -> float -> float

(** [student_quantile k p] computes [x] such that
    [student_cdf k x = p].

    @raise Invalid_argument if [p] is outside [0,1]. *)
val student_quantile : k:int -> float -> float
