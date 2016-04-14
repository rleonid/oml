(** Train a
    {{:https://en.wikipedia.org/wiki/Linear_discriminant_analysis#Multiclass_LDA}
    Multiclas LDA model by estimating a mean vector for the features (per
    class) and a common covariance matrix (common to all classes), which are
    then used to model a {{:https://en.wikipedia.org/wiki/Multivariate_normal_distribution}
    Multivariate normal distribution}. These, per class, distributions are used
    in Bayes's rule for classification. *)
module LDA(D: Intf.Continuous_encoded_data) : sig
  include Intf.Generative with type feature = D.feature
                          and type clas = D.clas
                          and type feature_probability = float

  val opt : ?shrinkage:float -> unit -> opt

end

module QDA(D: Intf.Continuous_encoded_data) : sig
  include Intf.Generative with type feature = D.feature
                          and type clas = D.clas
                          and type feature_probability = float

  val opt : ?normalize:bool -> ?shrinkage:float -> unit -> opt

end
