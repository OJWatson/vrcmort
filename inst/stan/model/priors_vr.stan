  // ----------------------------
  // Priors
  // ----------------------------

  // Mortality baseline
  alpha0 ~ normal(prior_alpha0_loc, prior_alpha0_scale);
  to_vector(alpha_age) ~ normal(0, prior_alpha_age_scale);
  to_vector(alpha_sex) ~ normal(0, prior_alpha_sex_scale);

  // Raw random effects (standard normal)
  to_vector(u_lambda_raw) ~ normal(0, 1);
  to_vector(u_rho_raw) ~ normal(0, 1);

  // Optional region-varying conflict slopes (raw)
  to_vector(beta_conf_re_raw) ~ normal(0, 1);
  to_vector(gamma_conf_re_raw) ~ normal(0, 1);

  // Hyperpriors for region-varying conflict slope scales
  sigma_beta_conf ~ normal(0, prior_sigma_beta_conf_scale);
  sigma_gamma_conf ~ normal(0, prior_sigma_gamma_conf_scale);

  // Optional region-specific time RW deviations (raw)
  for (g in 1:G) {
    to_vector(v_lambda_region_eps[g]) ~ normal(0, 1);
    to_vector(v_rho_region_eps[g]) ~ normal(0, 1);
  }
  sigma_v_lambda_region ~ normal(0, prior_sigma_v_lambda_region_scale);
  sigma_v_rho_region ~ normal(0, prior_sigma_v_rho_region_scale);

  // National time RW innovations (raw)
  for (g in 1:G) {
    v_lambda_eps[g] ~ normal(0, 1);
    v_rho_eps[g] ~ normal(0, 1);
  }

  // Hyperpriors for mortality time/region scales
  sigma_u_lambda ~ normal(0, prior_sigma_u_lambda_scale);
  sigma_v_lambda ~ normal(0, prior_sigma_v_lambda_scale);

  // Hyperpriors for reporting time/region scales
  sigma_u_rho ~ normal(0, prior_sigma_u_rho_scale);
  sigma_v_rho ~ normal(0, prior_sigma_v_rho_scale);

  // Conflict effects
  // Note: beta_conf is constrained >= 0 in the parameter block.
  beta_conf ~ normal(prior_beta_conf_loc, prior_beta_conf_scale);
  gamma_conf ~ normal(prior_gamma_conf_loc, prior_gamma_conf_scale);

  // Additional covariate effects
  if (K_mort > 0) {
    for (g in 1:G) {
      for (k in 1:K_mort) {
        beta_mort[g, k] ~ normal(prior_beta_mort_loc[k], prior_beta_mort_scale[k]);
      }
    }
  }

  if (K_rep > 0) {
    for (g in 1:G) {
      for (k in 1:K_rep) {
        gamma_rep[g, k] ~ normal(prior_gamma_rep_loc[k], prior_gamma_rep_scale[k]);
      }
    }
  }

  // Reporting anchors
  kappa0 ~ normal(prior_kappa0_loc, prior_kappa0_scale);
  kappa_post ~ normal(prior_kappa_post_loc, prior_kappa_post_scale);

  // Age-selective reporting penalty (half-normal via <lower=0>)
  delta_age_incr ~ normal(0, prior_delta_age_incr_scale);
  delta_age_scale ~ normal(0, prior_delta_age_scale_scale);

  // Dispersion (NB2)
  phi ~ exponential(prior_phi_rate);

  // Labeling probability (MAR)
  omega ~ beta(prior_omega_a, prior_omega_b);
