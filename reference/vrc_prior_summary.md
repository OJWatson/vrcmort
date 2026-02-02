# Summarise priors used by a fitted model

Summarise priors used by a fitted model

## Usage

``` r
vrc_prior_summary(x)
```

## Arguments

- x:

  A `vrcfit` object (recommended) or a resolved prior list as returned
  by the internal
  [`vrc_resolve_priors()`](https://github.com/OJWatson/vrcmort/reference/vrc_resolve_priors.md).

## Value

A data.frame with one row per prior hyperparameter group.
