
(** [linear (x1,y1) (x2,y2)] fits a line (l) between the two points such that
    given an x value one can get the y along that line with [l x]. *)
val linear : float * float -> float * float -> (float -> float)

module Spline : sig
  type cubic_spline_boundary_condition = NaturalCubic | Clamped
  type t (*= float * float * float * float * float) array *)
  val eval_at : t -> int -> float -> float
  val eval : t -> float -> float
  val eval_arr : t -> float array -> float array
  val fit : ?sorted:bool -> 'a -> (float * float) array -> t
  val lagrange : (float * float) array -> float -> float
end
