  vector[N * R] mu_rep;
  vector[N * R] rho_rep;
  vector[N * R] lambda_rep;
  vector[N * R] log_lik;
  array[N * R] int y_rep;

  vector[N] log_lik_miss;

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

      if (g == 2) {
        logit_rho_r += - delta_age[a] * post[t];
      }

      lambda_rep[i] = exp(log_lambda_r);
      rho_rep[i] = inv_logit(logit_rho_r);
      mu_rep[i] = exposure[j, r] * lambda_rep[i] * rho_rep[i] * (use_mar_labels == 1 ? omega : 1.0);

      // Posterior predictive draw
      y_rep[i] = neg_binomial_2_rng(mu_rep[i] + 1e-15, phi[g]);

      // Pointwise log-likelihood (for LOO / WAIC)
      log_lik[i] = neg_binomial_2_lpmf(y[j, r] | mu_rep[i] + 1e-15, phi[g]);
      
      mus[r] = exposure[j, r] * lambda_rep[i] * rho_rep[i];
    }

    if (use_mar_labels == 1) {
      log_lik_miss[j] = neg_binomial_2_sum_lpmf(y_miss[j] | mus * (1 - omega), phi[g]);
    } else {
      log_lik_miss[j] = 0;
    }
  }
