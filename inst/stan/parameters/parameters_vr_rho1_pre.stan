  // ----------------------------
  // Mortality process: log lambda
  // ----------------------------
  vector[G] alpha0;                    // cause-specific intercept
  matrix[A, G] alpha_age;              // age effects by cause
  matrix[S, G] alpha_sex;              // sex effects by cause

  matrix[R, G] u_lambda_raw;           // region RE (raw), will be centred and scaled
  array[G] vector[T] v_lambda_eps;     // time RW1 innovations (raw)

  vector<lower=0>[G] sigma_u_lambda;
  vector<lower=0>[G] sigma_v_lambda;

  vector<lower=0>[G] beta_conf;        // constrain >= 0 to avoid "conflict reduces mortality" pathology
  // Optional region-varying conflict effect (random slope by region)
  matrix[R, G] beta_conf_re_raw;
  vector<lower=0>[G] sigma_beta_conf;
  matrix[G, K_mort] beta_mort;         // additional mortality covariate effects

  // ----------------------------
  // Reporting process: logit rho
  // ----------------------------
  // In this model variant, pre-conflict reporting is fixed to 1 (rho = 1).
  // The reporting intercept below therefore describes the post-conflict baseline.
  vector[G] kappa_post;

  matrix[R, G] u_rho_raw;              // region RE (raw)
  array[G] vector[T] v_rho_eps;        // time RW1 innovations (raw)

  vector<lower=0>[G] sigma_u_rho;
  vector<lower=0>[G] sigma_v_rho;

  vector[G] gamma_conf;                // conflict effect on reporting
  // Optional region-varying conflict effect (random slope by region)
  matrix[R, G] gamma_conf_re_raw;
  vector<lower=0>[G] sigma_gamma_conf;
  matrix[G, K_rep] gamma_rep;          // additional reporting covariate effects

  // Optional region-specific time random walks (deviations around the national trend)
  array[G] matrix[T, R] v_lambda_region_eps;
  vector<lower=0>[G] sigma_v_lambda_region;

  array[G] matrix[T, R] v_rho_region_eps;
  vector<lower=0>[G] sigma_v_rho_region;

  // Age-selective post-conflict reporting drop for non-trauma:
  // build a monotone increasing "age penalty" then apply it negatively post-conflict.
  vector<lower=0>[A-1] delta_age_incr; // increments between age groups (positive)
  real<lower=0> delta_age_scale;

  // Observation dispersion (NB2)
  vector<lower=0>[G] phi;
