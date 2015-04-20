
(** [svd m] separate [m] into its singular value decomposition. *)
val svd : Matrices.t -> Matrices.t * Vectors.t * Matrices.t

(** [solve_linear a b] solve for [x] in [a x = b] using principle components
    regression. *)
val solve_linear : Matrices.t -> Vectors.t -> Vectors.t

(** [solve_linear_with_covariance a b] solve for [x] in [a x = b] and the covariance
    matrix [A * A^t]. *)
val solve_linear_with_covariance : Matrices.t -> Vectors.t -> Vectors.t * Matrices.t
