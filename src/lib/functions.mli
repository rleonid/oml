(** Implementations of basic functions needed to compute distributions.

  At the present moment many functions are wrappers to the Cephes library
  (http://www.netlib.org/cephes/), via a Ctypes interface implemented in
  the Ocephes library.
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

val chi_square_less : float -> int -> float
val chi_square_greater : float -> int -> float
val t_lookup : float -> int -> float

(** [softmax ?temperature weights] transforms [weights] into softmax weights dependent
    on [temperature].

    @raise Invalid_argument if [weights] is empty or [temperature = 0]. *)
val softmax : ?temperature:float -> float array -> float array
