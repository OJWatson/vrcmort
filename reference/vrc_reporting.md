# Reporting model component specification

Create a reporting component specification for
[`vrcm()`](https://github.com/OJWatson/vrcmort/reference/vrcm.md). This
is a light wrapper around a formula describing additional reporting
covariates (beyond the always-included conflict proxy).

The formula should not include `conflict`; conflict is included in the
core Stan model as `gamma_conf`.

## Usage

``` r
vrc_reporting(
  formula = ~1,
  conflict = c("fixed", "region"),
  time = c("national", "region")
)
```

## Arguments

- formula:

  A model formula. The default `~ 1` specifies no additional covariates.

- conflict:

  How to model the conflict effect in the reporting component. Use
  `"fixed"` for one effect shared across regions (per cause), or
  `"region"` for partial pooling (random slopes) by region (per cause).

- time:

  How to model time variation in the reporting component. `"national"`
  uses a single random walk shared across regions (per cause).
  `"region"` adds region-specific random walk deviations around the
  national trend.

## Value

An object of class `vrc_reporting`.
