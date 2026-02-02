# Mortality model component specification

Create a mortality component specification for
[`vrcm()`](https://github.com/OJWatson/vrcmort/reference/vrcm.md). This
is a light wrapper around a formula describing additional mortality
covariates (beyond the always-included conflict proxy).

The formula should not include `conflict`; conflict is included in the
core Stan model as `beta_conf`.

## Usage

``` r
vrc_mortality(
  formula = ~1,
  conflict = c("fixed", "region"),
  time = c("national", "region")
)
```

## Arguments

- formula:

  A model formula. The default `~ 1` specifies no additional covariates.

- conflict:

  How to model the conflict effect in the mortality component. Use
  `"fixed"` for one effect shared across regions (per cause), or
  `"region"` for partial pooling (random slopes) by region (per cause).

- time:

  How to model time variation in the mortality component. `"national"`
  uses a single random walk shared across regions (per cause).
  `"region"` adds region-specific random walk deviations around the
  national trend.

## Value

An object of class `vrc_mortality`.
