
(** [svd m] separate [m] into its singular value decomposition. *)
val svd : Matrices.t -> Matrices.t * Vectors.t * Matrices.t

(** [solve_linear a b] solve for [x] in [a x = b] using principle components
    regression. *)
val solve_linear : Matrices.t -> Vectors.t -> Matrices.t
