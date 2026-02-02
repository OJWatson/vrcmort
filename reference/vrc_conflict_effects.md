# Region-specific conflict effects

Extract posterior summaries of the conflict effect by region. This is
most useful when you fit a model with partial pooling of conflict
effects (random slopes by region), by setting `conflict = "region"` in
[`vrc_mortality()`](https://github.com/OJWatson/vrcmort/reference/vrc_mortality.md)
and/or
[`vrc_reporting()`](https://github.com/OJWatson/vrcmort/reference/vrc_reporting.md).

The underlying Stan model always defines region-specific conflict
effects (`beta_conf_rg` for mortality and `gamma_conf_rg` for
reporting). When the corresponding feature is disabled
(`conflict = "fixed"`), these region-specific effects are equal to the
global effect for all regions.

## Usage

``` r
vrc_conflict_effects(
  x,
  component = c("mortality", "reporting"),
  probs = c(0.1, 0.5, 0.9),
  draws = FALSE
)
```

## Arguments

- x:

  A `vrcfit` object.

- component:

  Which model component to summarise: mortality (`beta_conf_rg`) or
  reporting (`gamma_conf_rg`).

- probs:

  Quantiles to report.

- draws:

  Logical. If `FALSE` (default), returns posterior summaries. If `TRUE`,
  returns a long data.frame of posterior draws.

## Value

A data.frame. If `draws = FALSE`, columns include `mean`, `sd` and the
requested quantiles. If `draws = TRUE`, columns include `draw` and
`value`.
