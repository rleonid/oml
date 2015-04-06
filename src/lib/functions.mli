(** Implementations of basic functions needed to compute distributions. *)

val ln_gamma : float -> float
val ln_beta_func : float -> float -> float
val beta_func : float -> float -> float
(*val incomplete_beta_func : alpha:float -> beta:float -> float -> float *)
val gammap : float -> float -> float
val gammaq : float -> float -> float
val erf_taylor : float -> int -> float
val erf : float -> float
val erfc : float -> float
val chi_square_less : float -> int -> float
val chi_square_greater : float -> int -> float
val t_lookup : float -> int -> float
val softmax : ?temp:float -> float array -> int -> float
