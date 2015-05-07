
type 'a probabilities = ('a * float) list

val most_likely : 'a probabilities -> 'a

type ('cls, 'ftr) naive_bayes

type smoothing =
  { factor              : float
  ; feature_space_size  : int array
  }

val estimate : ?smoothing:smoothing -> feature_size:int ->
              ('ftr -> int array) -> ('cls * 'ftr) list ->
              ('cls, 'ftr) naive_bayes

val eval : ?bernoulli:bool -> ('cls, 'ftr) naive_bayes -> 'ftr -> 'cls probabilities

type 'cls gauss_bayes

val gauss_estimate : ('cls * float array) list -> 'cls gauss_bayes

val gauss_eval : 'cls gauss_bayes -> float array -> 'cls probabilities
