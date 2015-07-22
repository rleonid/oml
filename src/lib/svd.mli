
open Lacaml.D

type t

(** Accessors. *)
val u : t -> Matrices.t

val s : t -> Vectors.t

val vt : t -> Matrices.t

(** [svd m] separate [m] into its singular value decomposition.
  [mat] is destroyed in the process. *)
val svd : mat -> t

(** [solve_linear a b] solve for [x] in [a x = b] using principle components
    regression, when [a] is the Singular Value Decomposition. *)
val solve_linear : ?lambda:float -> t -> vec -> vec

(** [covariance_matrix a b] compute the covariance matrix [A * A^t]. *)
val covariance_matrix : ?lambda:float -> t -> mat

(** [looe t y] returns a function to efficiently compute the Leave-One-Out-Error
    vector, for computing the optimal regularization parameter. *)
val looe : t -> vec -> (float -> vec)

(** [h t] compute the Hat matrix of the original matrix passed to Svd. *)
val h : t -> mat

(** [h_diag t] compute the diagonal of the Hat matrix of the original matrix
    passed to Svd. *)
val h_diag : t -> vec
