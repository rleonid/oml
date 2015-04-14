(** Provide a thin matrix interfaces for [float array array]'s. *)

type matrix = Vectors.vector array

(** [equal d x y] two matrices are equal if they have the same dimensions
  and all pairwise elements are not [Util.significantly_different_from ?d]
  from each other. *)
val equal : ?d:float -> matrix -> matrix -> bool

(** [add x y] add two matrices. *)
val add : matrix -> matrix -> matrix

(** [sub x y] subtraction. *)
val sub : matrix -> matrix -> matrix

(** [mult s v] scalar multiplication. *)
val mult : float -> matrix -> matrix
