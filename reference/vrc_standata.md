# Build Stan data for the VR reporting model

Convert a long-format VR dataset into the list structure expected by the
Stan model shipped with the package.

The Stan model expects one row per cell: `region` x `time` x `age` x
`sex` x `cause`.

The minimal required columns are:

- `region`, `time`, `age`, `sex`, `cause`: identifiers

- `y`: observed VR counts (integer, can be `NA`)

- `exposure`: person-time at risk (numeric, \> 0). If `exposure` is
  absent, `pop` is used as a fallback.

- `conflict`: conflict intensity proxy (numeric)

Additional covariates can be included via `mortality_covariates` and
`reporting_covariates`.

## Usage

``` r
vrc_standata(
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
  priors = NULL,
  prior_PD = FALSE
)
```

## Arguments

- data:

  A data.frame in VR long format.

- t0:

  Conflict start time. May be an integer index, or a value that matches
  a value in `data$time`.

- mortality_covariates:

  Optional formula specifying additional covariates for the mortality
  (log-rate) component, excluding `conflict`.

- reporting_covariates:

  Optional formula specifying additional covariates for the reporting
  (logit-completeness) component, excluding `conflict`.

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

  Logical. If TRUE (recommended), standardise `conflict` and any
  covariate columns produced by the formulas.

- scale_binary:

  Logical. If TRUE, binary dummy columns in model matrices are also
  standardised. Defaults to FALSE.

- drop_na_y:

  Logical. If TRUE (default), rows with missing `y` are removed.

- duplicates:

  How to handle duplicate identifier rows. One of `"error"` (default) or
  `"sum"`.

- priors:

  Optional prior specification created by
  [`vrc_priors()`](https://github.com/OJWatson/vrcmort/reference/vrc_priors.md).
  If `NULL` (default), uses
  [`vrc_priors()`](https://github.com/OJWatson/vrcmort/reference/vrc_priors.md)
  with package defaults.

- prior_PD:

  Logical. If TRUE, Stan ignores the likelihood and samples from the
  prior. Useful for prior predictive checks.

## Value

A list with components:

- `standata`: list passed to Stan

- `df`: processed data used for fitting (subset if `drop_na_y`)

- `meta`: dimension metadata and level mappings

- `scaling`: scaling information for `conflict`, `X_mort`, and `X_rep`
