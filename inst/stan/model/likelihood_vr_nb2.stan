  // ----------------------------
  // Likelihood
  // ----------------------------
  if (prior_PD == 0) {
    for (j in 1:N) {
      int g = cause[j];
      int t = time[j];
      int a = age[j];
      int s = sex[j];

      vector[R] mus;
      for (r in 1:R) {
        int i = (j - 1) * R + r;
        real mort_x = 0;
        real rep_x = 0;
        if (K_mort > 0) mort_x = X_mort[i] * (beta_mort[g]');
        if (K_rep > 0)  rep_x  = X_rep[i]  * (gamma_rep[g]');

        real log_lambda_r = alpha0[g]
                          + alpha_age[a, g]
                          + alpha_sex[s, g]
                          + u_lambda[r, g]
                          + v_lambda[g][t]
                          + v_lambda_region[g][t, r]
                          + beta_conf_rg[r, g] * conflict[j, r]
                          + mort_x;

        real logit_rho_r = kappa0[g]
                         + kappa_post[g] * post[t]
                         + u_rho[r, g]
                         + v_rho[g][t]
                         + v_rho_region[g][t, r]
                         + gamma_conf_rg[r, g] * conflict[j, r]
                         + rep_x;

        // age-selective drop for non-trauma only (assume g==2)
        if (g == 2) {
          logit_rho_r += - delta_age[a] * post[t];
        }

        mus[r] = exposure[j, r] * exp(log_lambda_r) * inv_logit(logit_rho_r);
        
        if (use_mar_labels == 1) {
          y[j, r] ~ neg_binomial_2(mus[r] * omega, phi[g]);
        } else {
          y[j, r] ~ neg_binomial_2(mus[r], phi[g]);
        }
      }

      if (use_mar_labels == 1) {
        target += neg_binomial_2_sum_lpmf(y_miss[j] | mus * (1 - omega), phi[g]);
      }
    }
  }
