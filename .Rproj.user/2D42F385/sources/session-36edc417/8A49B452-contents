  // RW1 built from iid eps, then centred to sum-to-zero (identifiability)
  vector rw1_centered(vector eps, real sigma) {
    int T = num_elements(eps);
    vector[T] x;
    x[1] = eps[1] * sigma;
    for (t in 2:T) x[t] = x[t-1] + eps[t] * sigma;
    x = x - mean(x);
    return x;
  }
