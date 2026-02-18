  int<lower=1> N;                      // number of observed cells
  int<lower=1> R;
  int<lower=1> T;
  int<lower=1> A;
  int<lower=1> S;
  int<lower=1> G;                      // e.g. 2: trauma, non-trauma

  array[N] int<lower=1, upper=R> region;
  array[N] int<lower=1, upper=T> time;
  array[N] int<lower=1, upper=A> age;
  array[N] int<lower=1, upper=S> sex;
  array[N] int<lower=1, upper=G> cause;

  array[N] int<lower=0> y;
  // person-time at risk (for example, population * days_in_period)
  vector<lower=0>[N] exposure;

  // conflict intensity proxy (recommend standardising in R)
  vector[N] conflict;

  // Optional structured extensions (set by the R interface)
  // 1 = enabled, 0 = disabled
  int<lower=0, upper=1> use_beta_conf_re;     // region-varying conflict effect in mortality
  int<lower=0, upper=1> use_gamma_conf_re;    // region-varying conflict effect in reporting
  int<lower=0, upper=1> use_rw_region_lambda; // region-specific time RW (mortality)
  int<lower=0, upper=1> use_rw_region_rho;    // region-specific time RW (reporting)

  // additional covariates
  int<lower=0> K_mort;
  matrix[N, K_mort] X_mort;

  int<lower=0> K_rep;
  matrix[N, K_rep] X_rep;

  // post-conflict indicator by time index
  array[T] int<lower=0, upper=1> post;
  int<lower=1, upper=T> t0;

  // prior predictive mode
  int<lower=0, upper=1> prior_PD;

  // include age reporting breakdown
  int<lower=1, upper=G> age_drop_cause;

  // ----------------------------
  // Prior hyperparameters (supplied by R)
  // ----------------------------
  vector[G] prior_alpha0_loc;
  vector<lower=0>[G] prior_alpha0_scale;
  real<lower=0> prior_alpha_age_scale;
  real<lower=0> prior_alpha_sex_scale;

  vector[G] prior_beta_conf_loc;
  vector<lower=0>[G] prior_beta_conf_scale;
  vector[K_mort] prior_beta_mort_loc;
  vector<lower=0>[K_mort] prior_beta_mort_scale;

  vector<lower=0>[G] prior_sigma_u_lambda_scale;
  vector<lower=0>[G] prior_sigma_v_lambda_scale;
  vector<lower=0>[G] prior_sigma_beta_conf_scale;
  vector<lower=0>[G] prior_sigma_v_lambda_region_scale;

  vector[G] prior_kappa0_loc;
  vector<lower=0>[G] prior_kappa0_scale;
  vector[G] prior_kappa_post_loc;
  vector<lower=0>[G] prior_kappa_post_scale;

  vector[G] prior_gamma_conf_loc;
  vector<lower=0>[G] prior_gamma_conf_scale;
  vector[K_rep] prior_gamma_rep_loc;
  vector<lower=0>[K_rep] prior_gamma_rep_scale;

  vector<lower=0>[G] prior_sigma_u_rho_scale;
  vector<lower=0>[G] prior_sigma_v_rho_scale;
  vector<lower=0>[G] prior_sigma_gamma_conf_scale;
  vector<lower=0>[G] prior_sigma_v_rho_region_scale;

  real<lower=0> prior_delta_age_incr_scale;
  real<lower=0> prior_delta_age_scale_scale;
  
  vector<lower=0>[G] prior_phi_rate;
