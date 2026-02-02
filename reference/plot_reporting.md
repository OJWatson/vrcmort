# Plot estimated reporting completeness over time

Convenience plotting helper for visualising estimated reporting
completeness over time, aggregated over age and sex.

## Usage

``` r
plot_reporting(
  x,
  group_vars = c("region", "cause"),
  weight_var = "exposure",
  probs = c(0.1, 0.9),
  ribbon = TRUE,
  ...
)
```

## Arguments

- x:

  A `vrcfit` object.

- group_vars:

  Character vector of columns to group by in addition to `time`.
  Defaults to `c("region", "cause")`.

- weight_var:

  Column name used as weights when aggregating completeness across
  age/sex. Defaults to `"exposure"`.

- probs:

  Quantiles to plot (must match those computed by
  [`posterior_reporting()`](https://github.com/OJWatson/vrcmort/reference/posterior_reporting.md)).

- ribbon:

  Logical. If TRUE, draws a ribbon between the lowest and highest prob
  quantiles.

- ...:

  Additional args passed to
  [`ggplot2::theme()`](https://ggplot2.tidyverse.org/reference/theme.html).

## Value

A ggplot object.
