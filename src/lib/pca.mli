(** Principle Components Analysis. *)
open Lacaml.D

type t

(** [variances t] returns a vector of the amount of variance explained by each
    PC in [t]. *)
val variances : t -> float array

(** [components t] *)
val components : t -> float array array

(** [scalings t] returns an array of tuples representing the subtracted mean and
    divided standard deviation applied to the data before broken into PC's. *)
val scalings : t -> (float * float) array

(** [pca ?demean ?scale ?unbiased data] break down [data] into its principle
    components. [demean] will subtract the mean from each column (defaults
    to [true]). [scale] will divide each column by the standard deviation
    (defaults to [true]) and unbiased controls whether to use the unbaised
    standard deviation calculation (defaults to [true]).
 *)
val pca : ?demean:bool -> ?scale:bool -> ?unbiased:bool -> mat -> t

type pca_reduction_method =
  | SpecificNumberOfComponents of int     (** Only use this many components. *)
  | VarianceExplained of float            (** Choose as many components as needed to explain this much variance. *)

(** [reduce t prm] restrict the number of stored principle components via [prm].*)
val reduce : t -> pca_reduction_method -> t
