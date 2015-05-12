
(** The classifiers below assign a discrete probability distribution over the
    list of class 'a in their training set. *)
type 'a probabilities = ('a * float) list

(** [most_likely probabilities] returns the most likely class from the
    discrete probability distribution. *)
val most_likely : 'a probabilities -> 'a

(** A discrete Naive Bayes classifier of class ['cls] by observing
    features ['ftr]. *)
type ('cls, 'ftr) naive_bayes

(** When estimating a probability distribution by counting observed instances
    in the feature space we may want to smooth the values, particularly if our
    training data is sparse.

    [http://en.wikipedia.org/wiki/Additive_smoothing]
  *)
type smoothing =
  { factor              : float     (** Multiplicative factor *)
  ; feature_space_size  : int array (** Size of the space of each feature.
                                        Must be at least [feature_size] long.*)
  }

(** [estimate smoothing feature_size to_feature_array training_data] trains a
    discrete Naive Bayes classifier based on the [training_data].
    [to_feature_array] maps a feature to an integer array of length
    [feature_size]. Optionally, additive [smoothing] is applied to the final
    estimates if provided.
*)
val estimate : ?smoothing:smoothing -> feature_size:int ->
              ('ftr -> int array) -> ('cls * 'ftr) list ->
              ('cls, 'ftr) naive_bayes

(** [eval bernoulli classifier feature] classifies [feature]
    according to [classifier]. if [bernoulli] is specified we treat the
    underlying distribution as Bernoulli (as opposed to Multinomial) and
    estimate the likelihood with (1-p_i) for features [i] that are missing
    from [feature].
*)
val eval : ?bernoulli:bool -> ('cls, 'ftr) naive_bayes -> 'ftr -> 'cls probabilities

(** A continuous Gaussian Naive Bayes classifier of class ['cls]. The
    feature space is assumed to be a float array. *)
type 'cls gauss_bayes

(** [gauss_estimate training_data] trains a Gaussian Naive Bayes classifier from
    [training_data], where all of the data are of the same length; feature size. *)
val gauss_estimate : ('cls * float array) list -> 'cls gauss_bayes

(** [gauss_eval classifier feature] classify the [feature] using the [classifier]. *)
val gauss_eval : 'cls gauss_bayes -> float array -> 'cls probabilities
