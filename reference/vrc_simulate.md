# Simulate VR mortality with conflict-driven reporting collapse

Simulate a long-format vital registration (VR) dataset with a latent
mortality process and an explicit observation (reporting) process that
can collapse after a conflict start time.

The simulator generates:

- latent cause-specific mortality rates (by region, time, age, sex),

- true deaths given population denominators,

- observed VR counts given cause- and age-specific reporting
  completeness,

- optional misclassification between trauma and non-trauma,

- optional additional missingness mechanisms to stress test inference.

## Usage

``` r
vrc_simulate(
  R = 5,
  T = 120,
  age_breaks = c(0, 5, 15, 25, 35, 45, 55, 65, Inf),
  sexes = c("F", "M"),
  t0 = floor(T/2),
  seed = 1,
  missing = list(type = "none", block_intercept = -2.5, block_conflict_coef = 0.8,
    block_facility_coef = -1, age_dropout_strength = 0, age_dropout_old_from = 6,
    age_dropout_post_only = TRUE, mnar_strength = 0, missing_to_zero = FALSE),
  pop_total_range = c(3e+05, 1200000),
  pop_error_sd = 0.05,
  displacement_post_sd = 0.02,
  conflict_post_mean = 1,
  conflict_post_sd = 0.6,
  conflict_spike_prob = 0.08,
  conflict_spike_mean = 2,
  facility_pre_mean = 0.9,
  facility_post_drop = 0.35,
  facility_noise_sd = 0.08,
  beta_conf_true = c(1, 0.2),
  beta_conf_region_sd = c(0, 0),
  beta_fac_mort_true = c(0, -0.1),
  sigma_u_lambda_true = c(0.35, 0.25),
  sigma_v_lambda_true = c(0.08, 0.05),
  sigma_v_lambda_region_true = c(0, 0),
  phi_true = c(30, 60),
  rho0_true = c(0.7, 0.9),
  kappa_post_true = c(0, -1),
  gamma_conf_true = c(-0.1, -0.5),
  gamma_conf_region_sd = c(0, 0),
  gamma_fac_true = c(0.6, 0.8),
  sigma_u_rho_true = c(0.4, 0.35),
  sigma_v_rho_true = c(0.12, 0.08),
  sigma_v_rho_region_true = c(0, 0),
  age_penalty_non = NULL,
  misclass = list(p_non_to_trauma = 0.04, p_trauma_to_non = 0.01)
)
```

## Arguments

- R:

  Number of regions.

- T:

  Number of time points (for example, months).

- age_breaks:

  Numeric vector of age breakpoints. Defaults to 8 age groups.

- sexes:

  Character vector of sex labels.

- t0:

  Integer time index at which conflict begins.

- seed:

  Random seed.

- missing:

  A list controlling additional missingness mechanisms. See Details.

- pop_total_range:

  Length-2 numeric vector giving the range of total region populations
  to sample from.

- pop_error_sd:

  Numeric. Lognormal measurement error (standard deviation on the log
  scale) applied to the population denominator used for inference.

- displacement_post_sd:

  Numeric. Random-walk volatility controlling region population scaling
  after conflict (approximates displacement).

- conflict_post_mean:

  Numeric. Mean of the conflict covariate after conflict start.

- conflict_post_sd:

  Numeric. Standard deviation of the conflict covariate after conflict
  start.

- conflict_spike_prob:

  Numeric in (0,1). Probability of an additional conflict spike at a
  given time point after conflict start.

- conflict_spike_mean:

  Numeric. Mean spike size (gamma-distributed).

- facility_pre_mean:

  Numeric. Mean facility functioning score pre-conflict (bounded to
  0-1).

- facility_post_drop:

  Numeric. Average drop in facility functioning at conflict start.

- facility_noise_sd:

  Numeric. Noise SD used when generating facility functioning.

- beta_conf_true:

  Length-2 numeric vector. True effect of the conflict covariate on log
  mortality rates for trauma and non-trauma.

- beta_conf_region_sd:

  Length-2 numeric vector. Standard deviation of region-level deviations
  for the conflict effect in the mortality model.

- beta_fac_mort_true:

  Length-2 numeric vector. True effect of facility functioning on log
  mortality rates.

- sigma_u_lambda_true:

  Length-2 numeric vector. True SD of region random intercepts in the
  mortality model.

- sigma_v_lambda_true:

  Length-2 numeric vector. True SD of national RW1 time effects in the
  mortality model.

- sigma_v_lambda_region_true:

  Length-2 numeric vector. True SD of region-specific RW1 deviations
  around the national time effect.

- phi_true:

  Length-2 numeric vector. NB2 size (overdispersion) parameters for
  trauma and non-trauma deaths.

- rho0_true:

  Length-2 numeric vector. Baseline (pre-conflict) reporting
  completeness for trauma and non-trauma.

- kappa_post_true:

  Length-2 numeric vector. Post-conflict shift on the logit reporting
  scale for trauma and non-trauma.

- gamma_conf_true:

  Length-2 numeric vector. True effect of conflict on reporting
  completeness (logit scale).

- gamma_conf_region_sd:

  Length-2 numeric vector. Standard deviation of region-level deviations
  for the conflict effect in the reporting model.

- gamma_fac_true:

  Length-2 numeric vector. True effect of facility functioning on
  reporting completeness (logit scale).

- sigma_u_rho_true:

  Length-2 numeric vector. True SD of region random intercepts in the
  reporting model.

- sigma_v_rho_true:

  Length-2 numeric vector. True SD of national RW1 time effects in the
  reporting model.

- sigma_v_rho_region_true:

  Length-2 numeric vector. True SD of region-specific RW1 deviations
  around the national reporting time effect.

- age_penalty_non:

  Optional numeric vector of length equal to the number of age groups.
  If provided, gives the monotone age-selective reporting penalty for
  non-trauma post-conflict.

- misclass:

  A list with elements `p_non_to_trauma` and `p_trauma_to_non` giving
  misclassification probabilities among recorded deaths.

## Value

A list with elements:

- `df_full`: full long-format dataset (may contain `NA` in `y`).

- `df_obs`: subset of rows with observed `y`.

- `truth`: latent arrays and parameters used for simulation.

- `meta`: dimension metadata.

## Details

The `missing` argument allows additional missingness beyond the
reporting process. This is useful for stress testing identifiability.

Supported patterns:

- `type = "none"`: no additional missingness.

- `type = "block"`: region-time block dropout.

- `type = "age_selective"`: older-age dropout (typically for
  non-trauma).

- `type = "mnar"`: missing-not-at-random dropout driven by true death
  burden.

- `type = "combined"`: apply all three.

If `missing$missing_to_zero = TRUE`, missing cells are converted to
zeros (tests robustness to bad pipelines). Otherwise missing values
become `NA`.
