# Summarise covariate effects

Convenience helper to extract and summarise covariate effects from a
fitted model, including cause-specific conflict effects and any
additional covariate effects supplied via formulas.

By default this returns effects on the standardised covariate scale used
for fitting. If `original_scale = TRUE`, coefficients are rescaled back
to the original covariate scale using the centring/scaling information
stored in `fit$scaling`.

## Usage

``` r
vrc_coef_summary(x, probs = c(0.1, 0.5, 0.9), original_scale = FALSE)
```

## Arguments

- x:

  A `vrcfit` object.

- probs:

  Quantiles to include.

- original_scale:

  Logical. If TRUE, rescale coefficients back to the original covariate
  scale.

## Value

A data.frame.
