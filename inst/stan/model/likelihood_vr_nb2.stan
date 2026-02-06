  // ----------------------------
  // Likelihood
  // ----------------------------
  if (prior_PD == 0) {
    for (i in 1:N) {
      int g = cause[i];

      real mort_x = 0;
      real rep_x = 0;
      if (K_mort > 0) mort_x = X_mort[i] * (beta_mort[g]');
      if (K_rep > 0)  rep_x  = X_rep[i]  * (gamma_rep[g]');

      real log_lambda = alpha0[g]
                      + alpha_age[age[i], g]
                      + alpha_sex[sex[i], g]
                      + u_lambda[region[i], g]
                      + v_lambda[g][time[i]]
                      + v_lambda_region[g][time[i], region[i]]
                      + beta_conf_rg[region[i], g] * conflict[i]
                      + mort_x;

      real logit_rho = kappa0[g]
                     + kappa_post[g] * post[time[i]]
                     + u_rho[region[i], g]
                     + v_rho[g][time[i]]
                     + v_rho_region[g][time[i], region[i]]
                     + gamma_conf_rg[region[i], g] * conflict[i]
                     + rep_x;

      // age-selective drop for non-trauma only (assume g==2)
      if (g == 2) {
        logit_rho += - delta_age[age[i]] * post[time[i]];
      }

      real mu = exposure[i] * exp(log_lambda) * inv_logit(logit_rho);
      
      if (use_mar_labels == 1) {
        y[i] ~ neg_binomial_2(mu * omega, phi[g]);
      } else {
        y[i] ~ neg_binomial_2(mu, phi[g]);
      }
    }

    if (use_mar_labels == 1) {
      for (j in 1:N_miss) {
        int g = cause_miss[j];
        int t = time_miss[j];
        int a = age_miss[j];
        int s = sex_miss[j];

        real mort_x_miss = 0;
        real rep_x_miss = 0;
        if (K_mort > 0) mort_x_miss = X_mort_miss[j] * (beta_mort[g]');
        if (K_rep > 0)  rep_x_miss  = X_rep_miss[j]  * (gamma_rep[g]');

        vector[R] mus_miss;
        for (r in 1:R) {
          real log_lambda_r = alpha0[g]
                            + alpha_age[a, g]
                            + alpha_sex[s, g]
                            + u_lambda[r, g]
                            + v_lambda[g][t]
                            + v_lambda_region[g][t, r]
                            + beta_conf_rg[r, g] * conflict_miss[j, r]
                            + mort_x_miss;

          real logit_rho_r = kappa0[g]
                           + kappa_post[g] * post[t]
                           + u_rho[r, g]
                           + v_rho[g][t]
                           + v_rho_region[g][t, r]
                           + gamma_conf_rg[r, g] * conflict_miss[j, r]
                           + rep_x_miss;

          if (g == 2) {
            logit_rho_r += - delta_age[a] * post[t];
          }

          mus_miss[r] = exposure_miss[j, r] * exp(log_lambda_r) * inv_logit(logit_rho_r) * (1 - omega);
        }
        target += neg_binomial_2_sum_lpmf(y_miss[j] | mus_miss, phi[g]);
      }
    }
  }
