# Prior specification constructors

`vrcmort` uses a lightweight prior specification system inspired by the
`rstanarm` and `epidemia` packages. Priors are represented as simple
named lists and are converted into Stan hyperparameters by
[`vrc_standata()`](https://github.com/OJWatson/vrcmort/reference/vrc_standata.md).

Only a small set of distributions are currently supported because the
shipped Stan model uses conjugate-friendly priors:

- `normal(location, scale)`

- `exponential(rate)`

Create a bundled set of priors for use in
[`vrcm()`](https://github.com/OJWatson/vrcmort/reference/vrcm.md) /
[`vrc_fit()`](https://github.com/OJWatson/vrcmort/reference/vrc_fit.md).
The returned object is converted into Stan hyperparameters by
[`vrc_standata()`](https://github.com/OJWatson/vrcmort/reference/vrc_standata.md).

The defaults are designed to match the hard-coded priors used in earlier
versions of `vrcmort`.

## Usage

``` r
vrc_priors(
  beta_conf = normal(0.2, 0.3, autoscale = FALSE),
  beta_mort = normal(0, 0.3, autoscale = TRUE),
  gamma_conf = normal(0, 0.5, autoscale = FALSE),
  gamma_rep = normal(0, 0.5, autoscale = TRUE),
  kappa0 = NULL,
  kappa_post = normal(0, 0.7, autoscale = FALSE),
  alpha0 = normal(-9, 2, autoscale = FALSE),
  alpha_age = normal(0, 1, autoscale = FALSE),
  alpha_sex = normal(0, 0.5, autoscale = FALSE),
  sigma_u_lambda = normal(0, 0.5, autoscale = FALSE),
  sigma_v_lambda = normal(0, 0.2, autoscale = FALSE),
  sigma_beta_conf = normal(0, 0.3, autoscale = FALSE),
  sigma_v_lambda_region = normal(0, 0.2, autoscale = FALSE),
  sigma_u_rho = normal(0, 0.5, autoscale = FALSE),
  sigma_v_rho = normal(0, 0.2, autoscale = FALSE),
  sigma_gamma_conf = normal(0, 0.5, autoscale = FALSE),
  sigma_v_rho_region = normal(0, 0.2, autoscale = FALSE),
  delta_age_incr = normal(0, 0.5, autoscale = FALSE),
  delta_age_scale = normal(0, 1, autoscale = FALSE),
  phi = exponential(1)
)
```

## Arguments

- beta_conf:

  Prior for the conflict effect in the mortality component. This
  parameter is constrained to be non-negative in the base model.

- beta_mort:

  Prior for additional mortality covariate effects.

- gamma_conf:

  Prior for the conflict effect in the reporting component.

- gamma_rep:

  Prior for additional reporting covariate effects.

- kappa0:

  Optional prior for baseline reporting on the logit scale. If `NULL`
  (default), an anchored prior is used: cause 1 is centred on
  logit(0.70), cause 2 on logit(0.90), and remaining causes on
  logit(0.85).

- kappa_post:

  Prior for the post-conflict reporting shift.

- alpha0:

  Prior for the mortality intercept.

- alpha_age:

  Prior scale for age effects in the mortality component.

- alpha_sex:

  Prior scale for sex effects in the mortality component.

- sigma_u_lambda:

  Prior scale for region random effect scales (mortality).

- sigma_v_lambda:

  Prior scale for national time RW scales (mortality).

- sigma_beta_conf:

  Prior scale for region-varying conflict slope scales (mortality).

- sigma_v_lambda_region:

  Prior scale for region-specific time RW scales (mortality).

- sigma_u_rho:

  Prior scale for region random effect scales (reporting).

- sigma_v_rho:

  Prior scale for national time RW scales (reporting).

- sigma_gamma_conf:

  Prior scale for region-varying conflict slope scales (reporting).

- sigma_v_rho_region:

  Prior scale for region-specific time RW scales (reporting).

- delta_age_incr:

  Prior scale for increments in the monotone age penalty.

- delta_age_scale:

  Prior scale for the overall monotone age penalty scale.

- phi:

  Prior for the NB2 dispersion parameter.

## Value

A list of class `vrc_priors`.
