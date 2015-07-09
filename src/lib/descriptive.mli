val mean : float array -> float
val median : float array -> float
val var : float array -> float
val unbiased_var : float array -> float
val covariance : float array -> float array -> float
(** [correlation x y] returns the Pearson correlation coefficient of [x] and [y].*)
val correlation : float array -> float array -> float
val auto_correlation : int -> float array -> float
val moment : int -> float array -> float
val skew : float array -> float
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
