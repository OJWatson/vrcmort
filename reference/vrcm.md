# Fit a VR mortality + reporting model

High-level wrapper around
[`vrc_fit()`](https://github.com/OJWatson/vrcmort/reference/vrc_fit.md)
modelled after the user experience of packages like `epidemia` and
`rstanarm`. You specify a mortality component and a reporting component,
and `vrcm()` handles data preparation and model fitting.

The fitted model separates:

- a latent mortality process (`lambda`), and

- a reporting completeness process (`rho`).

Both processes include a conflict proxy by default.

## Usage

``` r
vrcm(
  mortality = vrc_mortality(~1),
  reporting = vrc_reporting(~1),
  data,
  t0,
  priors = NULL,
  ...
)
```

## Arguments

- mortality:

  A
  [`vrc_mortality()`](https://github.com/OJWatson/vrcmort/reference/vrc_mortality.md)
  specification, or a formula.

- reporting:

  A
  [`vrc_reporting()`](https://github.com/OJWatson/vrcmort/reference/vrc_reporting.md)
  specification, or a formula.

- data:

  A data.frame in canonical VR long format (see
  [`vrc_standata()`](https://github.com/OJWatson/vrcmort/reference/vrc_standata.md)).

- t0:

  Conflict start time (index or a value in `data$time`).

- priors:

  Optional prior specification created by
  [`vrc_priors()`](https://github.com/OJWatson/vrcmort/reference/vrc_priors.md).
  If `NULL` (default), uses
  [`vrc_priors()`](https://github.com/OJWatson/vrcmort/reference/vrc_priors.md)
  with package defaults.

- ...:

  Passed through to
  [`vrc_fit()`](https://github.com/OJWatson/vrcmort/reference/vrc_fit.md).

## Value

A `vrcfit` object, or (if `chains = 0`) a standata bundle returned by
[`vrc_standata()`](https://github.com/OJWatson/vrcmort/reference/vrc_standata.md).
