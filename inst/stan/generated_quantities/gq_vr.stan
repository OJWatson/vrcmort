  vector[N] mu_rep;
  vector[N] rho_rep;
  vector[N] lambda_rep;
  vector[N] log_lik;
  array[N] int y_rep;

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

    if (g == age_drop_cause) {
      logit_rho += - delta_age[age[i]] * post[time[i]];
    }

    lambda_rep[i] = exp(log_lambda);
    rho_rep[i] = inv_logit(logit_rho);
    mu_rep[i] = exposure[i] * lambda_rep[i] * rho_rep[i];

    // Posterior predictive draw
    y_rep[i] = neg_binomial_2_rng(mu_rep[i], phi[g]);

    // Pointwise log-likelihood (for LOO / WAIC)
    log_lik[i] = neg_binomial_2_lpmf(y[i] | mu_rep[i] + 1e-15, phi[g]);
  }
