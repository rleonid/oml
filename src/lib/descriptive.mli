(** Compute statistics (aka estimators) and other methods [ex. histogram] that
    describe data stored in a float array.

    Testing against statistics is found in [Inference]. These algorithms are
    not necessarily tailored for the most accurate algorithms that try to
    minimize truncating (see [Running] for online algorithms) but for having a
    simple function(al) interface with good runtime performance.
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

(** [population_var mean data] computes the variance of data when you know the
  population [mean]. *)
val population_var : float -> float array -> float

(* Between two random variables.*)

(** [covariance x y] computes the sample covariance of [x] and [y] where higher
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

(** [skew data] computes the sample skew of [data] (Fisher-Pearson's moment
    coefficient of skewness), which is a measure of asymmetry. For unimodal
    data negative values indicate that the left tail is longer in relation
    to the right.*)
val skew : float array -> float

(** [unbiased_skew data] adjusts the skew calculation to take into account
    sample size.

    The adjustments are chosen to prefer smaller mean squared error for small
    samples on non-normal distributions. See "Comparing measures of sample
    skewness and kurtosis" Joanes 1997, for details.  *)
val unbiased_skew : float array -> float

(** [kurtosis data] computes the sample kurtosis of [data]. This is a
    measure of the 'peakedness' vs 'tailness' of a distribution. It is adjusted
    so that a normal distribution will have kurtosis of 0. *)
val kurtosis : float array -> float

(** [unbiased_kurtosis data] adjusts the kurtosis calculation to take into
    account sample size.

    The adjustments are chosen to prefer smaller mean squared error for small
    samples on non-normal distributions. See "Comparing measures of sample
    skewness and kurtosis" Joanes 1997, for details.  *)
val unbiased_kurtosis : float array -> float

(* Error of measurements. *)

(** [var_standard_error data] computes the standard error of the variance
    of [data]. The standard error of a statistic (in this case variance)
    is the estimated standard deviation of that statistic. *)
val var_standard_error : float array -> float

(** [skew_standard_error data] computes the standard error of the sample
    (unbiased) skew statistic. *)
val skew_standard_error : float array -> float

(** [kurtosis_standard_error data] computes the standard error of the sample
    (unbiased) kurtosis statistic. *)
val kurtosis_standard_error : float array -> float

(* Statistics *)
(** [var_statistic data] computes the test statistic of the sample variance of
    [data]. This normalizes the variance measurement. *)
val var_statistic : float array -> float

(** [skew_statistic data] computes the test statistic of the sample skew of
    [data]. This normalizes the skew so that jjjj *)
val skew_statistic : float array -> float

(** [kurtosis_statistic data] computes the test statistic of the sample
    kurtosis of [data]. *)
val kurtosis_statistic : float array -> float

type skew_classification =
  [ `Negative | `Slightly_negative | `Normal | `Slightly_positive | `Positive ]

(** [classify_skew data] provides a terse description of the skewness of [data].

    Please note, even though a (polymorphic) variant is returned, caution must
    be taken when creating algorithms based upon these classification. These
    methods are brittle in the face of non-normal data and small sample size,
    although effort has been made to choose good practices.  *)
val classify_skew : float array -> skew_classification

type kurtosis_classification =
  [ `Skinny | `Slightly_skinny | `Fat | `Slightly_fat | `Normal ]

(** [classify_kurtosis data] provides a terse description of the kurtosis of
    [data]. The same warning as in [classify_skew] applies. *)
val classify_kurtosis : float array -> kurtosis_classification

(** Common statistics that describe data. *)
type commentary =
  { size     : int
  ; min      : float
  ; max      : float
  ; mean     : float
  ; std      : float
  ; var      : float
  ; skew     : float * skew_classification
  ; kurtosis : float * kurtosis_classification
  }

(** [unbiased_commentary data] summarizes [data] into a [commentary]. *)
val unbiased_commentary : float array -> commentary

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
