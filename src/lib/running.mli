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

(** [empty] an empty [t], useful for initializing the fold. *)
val empty : t

(** [init ?size x] initializes a [t] with [x] and initial [size]
    which defaults to [1]. *)
val init : ?size:int -> float -> t

(** [update ?size t x] incorporate [x] with [size] (defaulting to [1]) samples
    into the statistics tracked in [t]. *)
val update : ?size:int -> t -> float -> t

(** [join t1 t2] return a [Running.t] if you had first observed the elements
    by [t1] and then [t2]. *)
val join : t -> t -> t
