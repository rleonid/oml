(** A dimensional is uniform representation for arrays, vectors, row-vectors
    and matrices. *)
type t

(** Constructors. *)
val vector : float array -> t
val row_vector : float array -> t
val matrix : float array array -> t

(** Accessors. *)
val dimensions : t -> int * int

(** [equal d x y] two dimensionals are equal if they have the same dimensions and all
  pairwise elements are not [Util.significantly_different_from ?d] . *)
val equal : ?d:float -> t -> t -> bool

(** [add x y] add pairwise elements. *)
val add : t -> t -> t
(*
(** [sub x y] subtract pairwise elements. *)
val sub : t -> t -> t

(** [mult s v] multiplies each element of [v] by scalar [s]. *)
val mult : float -> t -> t

(** [dot x y] computes dot product. *)
val dot : t -> t -> float

(** [transpose t] switches the dimensions. *)
val transpose : t -> t
*)
