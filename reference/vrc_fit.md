# Fit the VR reporting model

Fit the hierarchical VR mortality model with an explicit reporting
process. The model is implemented in Stan and is fit using `rstan`.

## Usage

``` r
vrc_fit(
  data,
  t0,
  mortality_covariates = NULL,
  reporting_covariates = NULL,
  mortality_conflict = c("fixed", "region"),
  reporting_conflict = c("fixed", "region"),
  mortality_time = c("national", "region"),
  reporting_time = c("national", "region"),
  standardise = TRUE,
  scale_binary = FALSE,
  drop_na_y = TRUE,
  duplicates = c("error", "sum"),
  algorithm = c("sampling", "meanfield", "fullrank"),
  priors = NULL,
  prior_PD = FALSE,
  backend = c("rstan"),
  stan_model = "vr_reporting_model",
  ...
)
```

## Arguments

- data:

  A data.frame in long format (see
  [`vrc_standata()`](https://github.com/OJWatson/vrcmort/reference/vrc_standata.md)).

- t0:

  Conflict start time. See
  [`vrc_standata()`](https://github.com/OJWatson/vrcmort/reference/vrc_standata.md).

- mortality_covariates:

  Optional formula for additional mortality covariates, excluding
  `conflict`.

- reporting_covariates:

  Optional formula for additional reporting covariates, excluding
  `conflict`.

- mortality_conflict:

  How to model the conflict effect in the mortality component. Use
  `"fixed"` (default) for one effect shared across regions, or
  `"region"` for partial pooling (random slopes) by region.

- reporting_conflict:

  How to model the conflict effect in the reporting component. Use
  `"fixed"` (default) for one effect shared across regions, or
  `"region"` for partial pooling (random slopes) by region.

- mortality_time:

  How to model time variation in the mortality component. `"national"`
  uses a single random walk shared across regions; `"region"` adds
  region-specific random walk deviations around the national trend.

- reporting_time:

  How to model time variation in the reporting component. `"national"`
  uses a single random walk shared across regions; `"region"` adds
  region-specific random walk deviations around the national trend.

- standardise:

  Logical. If TRUE (recommended), standardise `conflict` and covariate
  columns in model matrices.

- scale_binary:

  Logical. If TRUE, also standardise binary dummy columns.

- drop_na_y:

  Logical. If TRUE, rows with missing `y` are removed before fitting.

- duplicates:

  How to handle duplicate identifier rows. One of `"error"` (default) or
  `"sum"`.

- algorithm:

  One of `"sampling"`, `"meanfield"`, or `"fullrank"` (passed to
  `rstan`).

- priors:

  Optional prior specification created by
  [`vrc_priors()`](https://github.com/OJWatson/vrcmort/reference/vrc_priors.md).
  If `NULL` (default), uses
  [`vrc_priors()`](https://github.com/OJWatson/vrcmort/reference/vrc_priors.md)
  with package defaults.

- prior_PD:

  Logical. If TRUE, samples from the prior distribution (no likelihood).

- backend:

  Backend to use. Currently only `"rstan"` is supported.

- stan_model:

  Stan model name. Defaults to `"vr_reporting_model"`.

- ...:

  Additional arguments passed to
  [`rstan::sampling`](https://mc-stan.org/rstan/reference/stanmodel-method-sampling.html)
  or
  [`rstan::vb`](https://mc-stan.org/rstan/reference/stanmodel-method-vb.html).

## Value

An object of class `vrcfit`.
