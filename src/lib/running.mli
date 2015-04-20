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

(** [init x] initialize a [t] with [x]. *)
val init : float -> t

(** [update t x] incorporate [x] into the statistics tracked in [t] *)
val update : t -> float -> t

(** [join t1 t2] return a running stat if you had first observed the elements
    by [t1] and then [t2]. *)
val join : t -> t -> t
