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
    
    // Check if all mus are effectively zero
    if (sum(mus) < 1e-12) {
      return y == 0 ? 0.0 : -1e15;
    }

    if (y == 0) {
      real log_p0 = 0;
      for (i in 1:R) {
        log_p0 += neg_binomial_2_lpmf(0 | mus[i], phi);
      }
      return log_p0;
    }

    vector[R] log_one_minus_p;
    for (r in 1:R) {
      // 1-p = mu / (mu + phi)
      log_one_minus_p[r] = log(mus[r] + 1e-15) - log(mus[r] + phi + 1e-15);
    }

    vector[y + 1] lp; // log probabilities
    lp[1] = 0;
    for (r in 1:R) {
      lp[1] += neg_binomial_2_lpmf(0 | mus[r], phi);
    }

    // C[j] = phi * sum( (1-p_r)^j )
    vector[y] log_C;
    for (j in 1:y) {
      vector[R] terms;
      for (r in 1:R) {
        terms[r] = j * log_one_minus_p[r];
      }
      log_C[j] = log(phi) + log_sum_exp(terms);
    }

    for (k in 1:y) {
      vector[k] components;
      for (j in 1:k) {
        components[j] = lp[k - j + 1] + log_C[j];
      }
      lp[k + 1] = log_sum_exp(components) - log(k);
    }

    return lp[y + 1];
  }
