(** Provide an interface to treat float array's as vectors. *)

type vector = float array

(** [equal d x y] two vectors are equal if they have the same length and all
  pairwise elements are not [Util.significantly_different_from ?d] . *)
val equal : ?d:float -> vector -> vector -> bool

(** [add x y] add pairwise elements. *)
val add : vector -> vector -> vector

(** [sub x y] subtract pairwise elements. *)
val sub : vector -> vector -> vector

(** [mult s v] multiplies each element of [v] by scalar [s]. *)
val mult : float -> vector -> vector

(** [dot x y] computes dot product. *)
val dot : vector -> vector -> float
