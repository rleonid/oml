(** Compute running statitics using recurrence equations. *)
type t = { size : int         (** Number of observations. *)
         ; last : float       (** Last observation. *)
         ; max : float        (** Maxiumum. *)
         ; min : float        (** Minimum. *)
         ; sum : float        (** Sum . *)
         ; sum_sq : float     (** Sum of squares. *)
         ; mean : float       (** Mean. *)
         ; var : float        (** _Unbiased_ variance. *)
         }

(** Defines a mean update function. Given an existing running
    statistic [t], number of samples [size], the already computed
    updated statistics [n_sum], [num_sum_sq], and [n_size], finally
    with the newly observed value, return a new estimate of the mean. *)
type mean_update = size:int -> n_sum:float ->
  n_sum_sq:float -> n_size:float -> t -> float -> float

(** [empty] an empty [t], useful for initializing the fold. *)
val empty : t

(** [init ?size x] initializes a [t] with [x] and initial [size]
    which defaults to [1]. *)
val init : ?size:int -> float -> t

(** [update ?size ?mean_update t x] incorporate [x] with given [size]
    (defaulting to [1]) and mean update rule [mean_update] (defaulting
    to unbiased) into the statistics tracked in [t]. *)
val update : ?size:int -> ?mean_update:mean_update -> t -> float -> t

(** [join t1 t2] return a [Running.t] if you had first observed the elements
    by [t1] and then [t2]. *)
val join : t -> t -> t
