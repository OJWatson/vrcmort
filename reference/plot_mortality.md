# Plot estimated mortality over time

Convenience plotting helper for visualising estimated latent mortality.
By default this plots expected true deaths (population x lambda),
aggregated over age and sex.

## Usage

``` r
plot_mortality(
  x,
  group_vars = c("region", "cause"),
  value = c("true_deaths", "rate"),
  ...
)
```

## Arguments

- x:

  A `vrcfit` object.

- group_vars:

  Character vector of columns to group by in addition to `time`.
  Defaults to `c("region", "cause")`.

- value:

  One of `"rate"` or `"true_deaths"`.

- ...:

  Additional arguments passed to
  [`ggplot2::theme()`](https://ggplot2.tidyverse.org/reference/theme.html).

## Value

A ggplot object.
