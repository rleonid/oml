(** Compute statistics (aka estimators) and other methods [ex. histogram] that
    describe data stored in a float array. 
    
    Test statistics are found in [Inference]. These algorithms are not
    necessarily tailored for the most accurate algorithms (see [Running]
    for online algorithms) but for having a simple function interface. 
*)

(* Central tendency *)

(** [mean data] returns the sample mean of [data]. *)
val mean : float array -> float

(** [median data] returns the number that separates [data] into two halfs of
    equal number where the lower half is less than the [median] and the higher
    half are greater.*)
val median : float array -> float

(** Spread. *)

(** [var data] returns the sample variance of [data]. *)
val var : float array -> float

(** [unbiased_var] returns the unbiased variance of [data] via Bessel's
    correction; ie. divinding by [n - 1]. *)
val unbiased_var : float array -> float

(** [population_var mean data] compute the variance of data when you know the
  population [mean]. *)
val population_var : float -> float array -> float

(* Between two random variables.*)

(** [covariance x y] compute the sample covariance of [x] and [y] where higher
    positive values indicate a direct relationship (larger values associate with
    larger values, smaller with smaller), a large negative value indicates an
    reverse relationship (smaller with larger and vice versa). While values
    close to 0.0 indicate no relationship.
    
    @raise Invalid_argument if the size of [x] doesn't equal the size of [y]. *)
val covariance : float array -> float array -> float

(** [correlation x y] returns the Pearson correlation coefficient of [x] and
    [y]. This is normalized sample covariance. *)
val correlation : float array -> float array -> float

(** [autocorrelation lag data] computes the correlation of [data] with itself
  offset by [lag].  *)
val autocorrelation : int -> float array -> float

(* Higher moments.*)

(** [moment k data] computes the [k]th sample central moment of [data]. *)
val moment : int -> float array -> float

(** [skew data] compute the sample skew of [data], which is a measure of
    asymmetry. Aka, Pearson's moment coefficient of skewness. *)
val skew : float array -> float

(** [kurtosis data] compute the sample kurtosis of [data]. *)
val kurtosis : float array -> float

val unbiased_skew : float array -> float
val unbiased_kurtosis : float array -> float
val var_standard_error : float array -> float
val skew_standard_error : float array -> float
val kurtosis_standard_error : float array -> float
val var_statistic : float array -> float
val skew_statistic : float array -> float
val kurtosis_statistic : float array -> float
val classify_skew : float array -> string
val classify_kurtosis : float array -> string
val stat_classify : float array -> (string * float) list
val unbiased_classify : float array -> (string * float) list
val unbiased_distribution_commentary : float array -> (string * float * string) list
type dist_stats = { size     : int
                  ; mean     : float
                  ; var      : float
                  ; skew     : float
                  ; kurtosis : float
                  }
val dist_classify : float array -> dist_stats
val unbiased_dist_classify : float array -> dist_stats

(** [histogram data width_setting] group [data] into a specific number of buckets
    of given width (according to [width_setting]:

    - [`Buckets n] create [n] equally sized bucketes to fit all the data.
    - [`Specific bkts] use [bkts] as lower boundaries on buckets.
    - [`Width w] create buckets of size [w] to fit all the data.
    *)
val histogram : [`Width of float | `Buckets of int | `Specific of float array]
                -> float array -> (float * int) array

(** [geometric_mean arr] is the product of the elements of [arr] raised to the
    power of the reciprocal of the length. *)
val geometric_mean : float array -> float

(*val geometric_mean_definitional  : float array -> float *)
(** [harmonic_mean arr] is the reciprocal of the arithmetic mean of the
    reciprocals of arr. *)
val harmonic_mean : float array -> float

(** [spearman x y] returns the Spearman rank correlation coefficient. *)
val spearman : float array -> float array -> float
