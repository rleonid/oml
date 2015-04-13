(** Probability density functions and cumulative distribution functions for
    some commont distributions. *)

(** [normal_cdf mean std x] probability that a normal random
    variable with [mean] and standard deviation [std] takes a value less than
    or equal to [x]. *)
val normal_cdf : mean:float -> std:float -> float -> float

(** [normal_pdf mean std x] value of the Normal distribution
    function (with [mean] and standard deviation [std]) at [x].*)
val normal_pdf : mean:float -> std:float -> float -> float

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
