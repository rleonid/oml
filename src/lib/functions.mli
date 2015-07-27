(** Implementations of basic functions needed to compute distributions.

  At the present moment many functions are wrappers to the Cephes library
  (http://www.netlib.org/cephes/), via a Ctypes interface implemented in
  the Ocephes library.
 *)

(** [ln_gamma x] compute the natural logarithm of the gamma function of [x].

    For positive integers [exp (ln_gamma x)] approximates [(x - 1)!], it is
    usually much more accurate to use [ln_gamma] instead of [gamma] and
    afterwards compute the exponent.*)
val ln_gamma : float -> float

val ln_beta : float -> float -> float
val beta : float -> float -> float
val regularized_beta : alpha:float -> beta:float -> ?epsilon:float ->
  ?max_iterations:int -> float -> float
val gammap : float -> float -> float
val gammaq : float -> float -> float
val erf : float -> float
val erfc : float -> float
val chi_square_less : float -> int -> float
val chi_square_greater : float -> int -> float
val t_lookup : float -> int -> float

(** [softmax ?temperature weights] transforms [weights] into softmax weights dependent
    on [temperature].

    @raise Invalid_argument if [weights] is empty or [temperature = 0]. *)
val softmax : ?temperature:float -> float array -> float array
