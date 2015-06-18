(** Provides various measurement functions from statistics and analysis *)


val normal_kl_divergence : p_mean:float -> p_sigma:float
  -> q_mean:float -> q_sigma:float -> float
(** Computes kl divergence of the normal distributions P and Q defined
    given means [p_mean] and [q_mean] and std deviations [p_sigma]
    and [q_sigma]. Returns infinity for variances near zero. *)
