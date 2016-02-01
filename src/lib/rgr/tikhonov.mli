(** Multi-dimensional input regression with a matrix regularizer.
  described {{:https://en.wikipedia.org/wiki/Tikhonov_regularization} here}.

  Please take care with using this method as not all of the algorithms have
  been verified. A warning is printed to standard-error. *)

type opt =
  { tik_matrix : float array array   (** The regularizing matrix. *)
  ; l2_regularizer : [`S of float | `From of float array] option  (** How to optionally determine the ridge parameter. *)
  }

val opt : ?tik_matrix:float array array ->
          ?l2_regularizer:[`S of float | `From of float array] ->
          unit ->
          opt

include Intf.Linear_model
  with type input = float array
  and type opt := opt

(** [aic linear_model] return the Akaike information criterion for the
    [linear_model].*)
val aic : t -> float

(** [press linear_model] return the Predicted REsidual Sum of Squares for the
    [linear_model]. *)
val press : t -> float
