  matrix[R, G] u_lambda;
  matrix[R, G] u_rho;
  array[G] vector[T] v_lambda;
  array[G] vector[T] v_rho;

  // Optional region-varying conflict effects
  matrix[R, G] beta_conf_rg;
  matrix[R, G] gamma_conf_rg;

  // Optional region-specific time random walk deviations (T x R for each cause)
  array[G] matrix[T, R] v_lambda_region;
  array[G] matrix[T, R] v_rho_region;

  vector[A] delta_age;                 // anchored at 0 for youngest

  // Centre region effects and scale
  for (g in 1:G) {
    u_lambda[, g] = (u_lambda_raw[, g] - mean(u_lambda_raw[, g])) * sigma_u_lambda[g];
    u_rho[, g]    = (u_rho_raw[, g] - mean(u_rho_raw[, g])) * sigma_u_rho[g];
  }

  // Centred RW1 time effects
  for (g in 1:G) {
    v_lambda[g] = rw1_centered(v_lambda_eps[g], sigma_v_lambda[g]);
    v_rho[g]    = rw1_centered(v_rho_eps[g], sigma_v_rho[g]);
  }

  // Region-specific time RW deviations (optional)
  for (g in 1:G) {
    if (use_rw_region_lambda == 1) {
      for (r in 1:R) {
        v_lambda_region[g][, r] = rw1_centered(v_lambda_region_eps[g][, r], sigma_v_lambda_region[g]);
      }
    } else {
      v_lambda_region[g] = rep_matrix(0, T, R);
    }

    if (use_rw_region_rho == 1) {
      for (r in 1:R) {
        v_rho_region[g][, r] = rw1_centered(v_rho_region_eps[g][, r], sigma_v_rho_region[g]);
      }
    } else {
      v_rho_region[g] = rep_matrix(0, T, R);
    }
  }

  // Region-varying conflict slopes (optional)
  for (g in 1:G) {
    real sb = sigma_beta_conf[g];
    real sg = sigma_gamma_conf[g];
    real mb = mean(beta_conf_re_raw[, g]);
    real mg = mean(gamma_conf_re_raw[, g]);

    for (r in 1:R) {
      if (use_beta_conf_re == 1) {
        // Positive random slopes via lognormal multiplier with E(multiplier)=1
        beta_conf_rg[r, g] = beta_conf[g] * exp(sb * (beta_conf_re_raw[r, g] - mb) - 0.5 * square(sb));
      } else {
        beta_conf_rg[r, g] = beta_conf[g];
      }

      if (use_gamma_conf_re == 1) {
        gamma_conf_rg[r, g] = gamma_conf[g] + sg * (gamma_conf_re_raw[r, g] - mg);
      } else {
        gamma_conf_rg[r, g] = gamma_conf[g];
      }
    }
  }

  // Monotone age penalty anchored at 0 for youngest age group
  delta_age[1] = 0;
  for (a in 2:A) {
    delta_age[a] = delta_age[a-1] + delta_age_incr[a-1];
  }
  delta_age = delta_age_scale * delta_age;
