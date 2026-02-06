  /**
   * Log PMF of the sum of independent Negative Binomial 2 variables
   * using an exact recurrence relation.
   *
   * @param y Observed sum
   * @param mus Vector of means for each component
   * @param phi Dispersion parameter (shared)
   * @return Log probability mass
   */
  real neg_binomial_2_sum_lpmf(int y, vector mus, real phi) {
    int R = num_elements(mus);
    
    // Base case: y = 0 (Probability all components are 0)
    if (y == 0) {
      real log_p0 = 0;
      for (r in 1:R) {
        log_p0 += neg_binomial_2_lpmf(0 | mus[r], phi);
      }
      return log_p0;
    }

    // Pre-calculate log(p) and log(1-p) for each component
    // In Stan's neg_binomial_2: p = phi / (phi + mu)
    vector[R] log_one_minus_p; 
    for (r in 1:R) {
      log_one_minus_p[r] = log(mus[r]) - log(mus[r] + phi);
    }

    // Recurrence array
    vector[y + 1] lp; 
    lp[1] = 0;
    for (r in 1:R) {
      lp[1] += neg_binomial_2_lpmf(0 | mus[r], phi);
    }

    // C[j] = phi * sum( (1-p_r)^j )
    // We vectorize the calculation of log_C to improve gradient speed
    vector[y] log_C;
    real log_phi = log(phi);
    for (j in 1:y) {
      log_C[j] = log_phi + log_sum_exp(j * log_one_minus_p);
    }

    // Main Recurrence: lp[k+1] is log(P(Sum = k))
    for (k in 1:y) {
      // Log-space convolution of previous probabilities and C coefficients
      vector[k] components;
      for (j in 1:k) {
        components[j] = lp[k - j + 1] + log_C[j];
      }
      lp[k + 1] = log_sum_exp(components) - log(k);
    }

    return lp[y + 1];
  }
