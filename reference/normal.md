# Normal prior

Normal prior

## Usage

``` r
normal(location = 0, scale = 1, autoscale = FALSE)
```

## Arguments

- location:

  Prior mean.

- scale:

  Prior standard deviation (\> 0).

- autoscale:

  Logical. If `TRUE`, the scale is adjusted by the column scale of the
  design matrix (similar to rstanarm/epidemia). With the default
  `standardise = TRUE`, autoscaling has no practical effect for
  continuous predictors.

## Value

A prior specification list.
