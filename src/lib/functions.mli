(** Implementations of basic functions needed to compute distributions. *)

(** [ln_gamma x] compute the natural logarithm of the gamma function of [x].

    For positive integers [exp (ln_gamma x)] approximates [(x - 1)!] *)
val ln_gamma : float -> float

val ln_beta_func : float -> float -> float
val beta_func : float -> float -> float
val regularized_beta : alpha:float -> beta:float -> ?epsilon:float ->
  ?max_iterations:int -> float -> float
val gammap : float -> float -> float
val gammaq : float -> float -> float
val erf_taylor : float -> int -> float
val erf : float -> float
val erfc : float -> float
val chi_square_less : float -> int -> float
val chi_square_greater : float -> int -> float
val t_lookup : float -> int -> float

(** [softmax ?temp weights] transform [weights] into softmax weights dependent
    on [temp]erature.

    @raise Invalid_argument if [weights] is empty or [temp = 0]. *)
val softmax : ?temp:float -> float array -> float array
