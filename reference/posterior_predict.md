# Posterior predictive distribution for observed counts

Extract posterior predictive draws `y_rep` from the Stan generated
quantities.

## Usage

``` r
posterior_predict(x, draws = FALSE, probs = c(0.1, 0.5, 0.9), ...)
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
