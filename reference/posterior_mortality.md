# Posterior summaries for latent mortality rates

Extract posterior draws/summaries for the cell-level latent mortality
rates `lambda` returned from Stan as a generated quantity
(`lambda_rep`).

## Usage

``` r
posterior_mortality(x, draws = FALSE, probs = c(0.1, 0.5, 0.9), ...)
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
