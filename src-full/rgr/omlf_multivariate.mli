
(** Multi-dimensional input regression, with support for Ridge regression. *)

type opt =
  { add_constant_column : bool  (** Instructs the method to efficiently insert a
                                    column of 1's into the design matrix for the
                                    constant term. *)
  ; l2_regularizer : [`S of float | `From of float array] option
                                (** How to optionally determine the ridge parameter. *)
  }

val opt : ?l2_regularizer:[`S of float | `From of float array] ->
          ?add_constant_column:bool ->
          unit ->
          opt

include Omlf_regression_interfaces.Linear_model
  with type input = float array
   and type opt := opt

(** [aic linear_model] return the Akaike information criterion for the
    [linear_model].*)
val aic : t -> float

(** [press linear_model] return the Predicted REsidual Sum of Squares for the
    [linear_model]. *)
val press : t -> float
