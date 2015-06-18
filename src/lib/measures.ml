
let normal_kl_divergence ~p_mean ~p_sigma ~q_mean ~q_sigma =
  let p_sigma_degen = not @@ Util.significantly_different_from p_sigma 0.0 in
  let q_sigma_degen = not @@ Util.significantly_different_from q_sigma 0.0 in
  if p_sigma_degen || q_sigma_degen then infinity else
  let mean_diff = p_mean -. q_mean in
  let mean_diff_sq = mean_diff *. mean_diff in
  let p_sigma_sq = p_sigma *. p_sigma in
  let q_sigma_sq = q_sigma *. q_sigma in
  log (q_sigma /. p_sigma) +. (p_sigma_sq +. mean_diff_sq) /.
      (2.0 *. q_sigma_sq) -. 0.5
