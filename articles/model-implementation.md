# Model Implementation

## Overview

This vignette explains how `vrcmort` turns a VR long table into a Stan
model fit.

The package follows an epidemia-like pattern:

1.  **Validate and index the data** into integer indices (region, time,
    age, sex, cause).
2.  **Construct design matrices** from user-provided formulas.
3.  **Build a Stan data list** that also contains prior hyperparameters.
4.  **Fit the model** via Stan (HMC via `rstan`).

The goal is that most users never need to edit Stan, while advanced
users can still do so.

## Canonical VR long format

The core fitting functions expect one row per cell. Columns are:

- identifiers: `region`, `time`, `age`, `sex`, `cause`
- outcome: `y` (observed VR deaths)
- denominator: `exposure` (person-time at risk)
- conflict covariate: `conflict`
- any additional covariates referenced in formulas

You can check the recommended contract with:

``` r
vrc_canonical_columns()
```

### Building VR long data from individual records

If you have individual-level deaths with region, month, age, sex, and
ICD-10 codes, the typical workflow is:

1.  classify ICD-10 into a small cause grouping (for example trauma vs
    non-trauma),
2.  bin ages into groups,
3.  aggregate counts by `(region, time, age, sex, cause)`,
4.  join population and covariates, and compute exposure.

The tutorial vignette *Data preparation from individual VR records*
contains a worked template.

## Validation and indexing

Before fitting, `vrcmort` can validate common pitfalls:

- duplicated cells,
- negative or non-integer counts,
- missing exposure,
- likely “missing-as-zero” pipelines.

``` r
vrc_validate_data(vr_long)
```

Then the package maps your identifiers to integer indices needed by
Stan:

``` r
idx <- vrc_index(vr_long)
str(idx$meta)
```

The `meta` object stores levels and dimensions, and is returned in
fitted objects.

## Formulas and design matrices

The Stan model always includes the conflict proxy in both the mortality
and reporting submodels.

User-supplied formulas specify **additional** covariates beyond
conflict.

For example:

``` r
mort <- vrc_mortality(~ facility + food_insecurity)
rep  <- vrc_reporting(~ facility)
```

Internally:

- [`vrc_standata()`](https://github.com/OJWatson/vrcmort/reference/vrc_standata.md)
  builds `X_mort` and `X_rep` using
  [`model.matrix()`](https://rdrr.io/r/stats/model.matrix.html).
- covariates are (by default) centred and scaled.
- the scaling information is stored in the fitted object so that
  coefficients can be reported on the original scale if desired.

### Structured options (partial pooling and region-specific trends)

The component constructors also control optional structured features:

- `conflict = "fixed"` or `"region"` for region-varying conflict
  effects,
- `time = "national"` or `"region"` for region-specific time random walk
  deviations.

These are passed to Stan as boolean flags:

- `use_beta_conf_re`
- `use_gamma_conf_re`
- `use_rw_region_lambda`
- `use_rw_region_rho`

## Building Stan data

To see what is passed to Stan without fitting, set `chains = 0`:

``` r
standata_bundle <- vrcm(
  mortality = mort,
  reporting = rep,
  data = vr_long,
  t0 = conflict_start_month,
  chains = 0
)

names(standata_bundle$standata)
```

This returns a list that contains:

- `standata`: the full Stan data list,
- `meta`: dimensions and factor levels,
- `scaling`: centring and scaling applied to covariates.

## Priors as data

A key design decision in `vrcmort` is that priors are provided to Stan
as data.

This means you can change priors without editing Stan files.

``` r
pri <- vrc_priors(
  beta_conf = normal(0.3, 0.2),
  gamma_conf = normal(0, 0.5),
  beta_mort = normal(0, 0.5, autoscale = TRUE),
  gamma_rep = normal(0, 0.7, autoscale = TRUE)
)

fit <- vrcm(
  mortality = mort,
  reporting = rep,
  data = vr_long,
  t0 = conflict_start_month,
  priors = pri,
  chains = 4,
  iter = 1000
)

vrc_prior_summary(fit)
```

The prior vignette explains how each prior maps to the model parameters.

## Stan model layout

The Stan code in `inst/stan/` is split into modular include files (“lego
bricks”):

- `functions/`: reusable helper functions (for example RW1 construction)
- `data/`: the data block, including prior hyperparameters
- `parameters/`: parameter declarations
- `tparameters/`: transformed parameters (linear predictors, expected
  mean)
- `model/`: priors and likelihood
- `generated_quantities/`: posterior predictive draws, pointwise
  log-likelihood

The top-level model file `vr_reporting_model.stan` mostly consists of
`#include` statements.

This makes it easier to create alternative variants (for example adding
misclassification) while keeping the same R interface.

## Fitting via rstan

Model fitting uses Hamiltonian Monte Carlo (HMC) as implemented in Stan.
The main user controls are:

- `chains`, `iter`, `seed`
- `adapt_delta`, `max_treedepth`

If you have difficult geometry (common in hierarchical models with weak
identifiability), increasing `adapt_delta` is often helpful.

``` r
fit <- vrcm(
  mortality = mort,
  reporting = rep,
  data = vr_long,
  t0 = conflict_start_month,
  chains = 4,
  iter = 2000,
  adapt_delta = 0.95,
  max_treedepth = 12
)
```

## Outputs

A fitted model (`vrcfit`) stores:

- the underlying `rstan` fit
- the `standata` used
- the `meta` indexing information

Common post-processing helpers include:

- [`posterior_reporting()`](https://github.com/OJWatson/vrcmort/reference/posterior_reporting.md)
  for $\,\rho$
- [`posterior_mortality()`](https://github.com/OJWatson/vrcmort/reference/posterior_mortality.md)
  for $\,\lambda$
- [`posterior_expected_counts()`](https://github.com/OJWatson/vrcmort/reference/posterior_expected_counts.md)
  for $\mu$
- [`plot_reporting()`](https://github.com/OJWatson/vrcmort/reference/plot_reporting.md)
  and
  [`plot_mortality()`](https://github.com/OJWatson/vrcmort/reference/plot_mortality.md)

For model comparison, the generated quantities include pointwise
`log_lik`, which can be used with the `loo` package.
