# Posterior summaries for reporting completeness

Extract posterior draws (or summaries) for the cell-level reporting
completeness `rho` returned from Stan as a generated quantity
(`rho_rep`).

## Usage

``` r
posterior_reporting(x, draws = FALSE, probs = c(0.1, 0.5, 0.9), ...)
```

## Arguments

- x:

  A `vrcfit` object.

- draws:

  Logical. If TRUE, returns a matrix of posterior draws with one column
  per observed cell.

- probs:

  Numeric vector of quantiles to compute.

- ...:

  Reserved for future use.

## Value

If `draws = TRUE`, a matrix with dimensions `n_draws x N`. Otherwise a
data.frame combining `x$data` with summary columns for `rho`.
