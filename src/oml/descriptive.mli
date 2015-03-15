val mean : float array -> float
val median : float array -> float
val var : float array -> float
val unbiased_var : float array -> float
val covariance : float array -> float array -> float
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
type dist_stats 
val dist_classify : float array -> dist_stats
val unbiased_dist_classify : float array -> dist_stats
val histogram : float array -> float -> (float * int) array
