(** Provide an interface to treat float array's as vectors. *)

type t = float array

(** [equal d x y] two vectors are equal if they have the same length and all
  pairwise elements are not [Util.significantly_different_from ?d] . *)
val equal : ?d:float -> t -> t -> bool

(** [add x y] add pairwise elements. *)
val add : t -> t -> t

(** [sub x y] subtract pairwise elements. *)
val sub : t -> t -> t

(** [mult s v] multiplies each element of [v] by scalar [s]. *)
val mult : float -> t -> t

(** [dot x y] computes dot product. *)
val dot : t -> t -> float
