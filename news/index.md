# Changelog

## vrcmort 0.0.9

- Added `pre_conflict_reporting` option to
  [`vrc_fit()`](https://github.com/OJWatson/vrcmort/reference/vrc_fit.md)
  to control model assumptions, allowing pre-conflict reporting to be
  estimated or fixed to 1.
- Added `generated_quantities` option to
  [`vrc_fit()`](https://github.com/OJWatson/vrcmort/reference/vrc_fit.md)
  for performance, allowing the GQ block to be skipped for faster
  sampling.

## vrcmort 0.0.8

- pkgdown website referencing completed
- Gihtub CI completed

## vrcmort 0.0.7

- Added support for user-supplied Stan files via
  [`vrc_model_spec()`](https://github.com/OJWatson/vrcmort/reference/vrc_model_spec.md)
  and
  [`vrc_write_stan_template()`](https://github.com/OJWatson/vrcmort/reference/vrc_write_stan_template.md).
- Posterior extraction helpers now respect a `param_map` so generated
  quantity names can be customised.
- Added basic CI configuration and improved R CMD check hygiene
  (`.Rbuildignore`, global variable declarations).
- Minor test updates for testthat edition 3.

## vrcmort 0.0.6

- Expanded documentation with epidemia-style vignettes covering the
  model introduction, mathematical description, implementation details,
  schematic, partial pooling, priors, and under-reporting with
  age-selective reporting collapse.
- Added two step-by-step tutorial vignettes (data preparation from
  individual records and covariate exploration).
- Updated pkgdown configuration to surface the new articles.

## vrcmort 0.0.5

- Added a lightweight prior specification system
  ([`vrc_priors()`](https://github.com/OJWatson/vrcmort/reference/vrc_priors.md))
  inspired by rstanarm/epidemia.
- Users can now override key priors (covariate coefficients, conflict
  effects, reporting anchors, random-effect scales, dispersion) via
  [`vrcm()`](https://github.com/OJWatson/vrcmort/reference/vrcm.md) /
  [`vrc_fit()`](https://github.com/OJWatson/vrcmort/reference/vrc_fit.md).
- Stan model priors are now parameterised through Stan `data`
  (hyperparameters) rather than hard-coded constants.

## vrcmort 0.0.4

- Added optional partial pooling for conflict effects by region (random
  slopes) in both the mortality and reporting components.
- Added optional region-specific time random walks (deviations around
  the national trend) for both mortality and reporting.
- Added helpers to extract region-specific conflict effects from fitted
  models.
- Fixed
  [`vrc_fit()`](https://github.com/OJWatson/vrcmort/reference/vrc_fit.md)
  to pass `duplicates` through to
  [`vrc_standata()`](https://github.com/OJWatson/vrcmort/reference/vrc_standata.md).

## vrcmort 0.0.3

- Added an epidemia-style high-level interface (`vrcm`) with explicit
  mortality and reporting components.
- Added convenience S3 methods for `vrcfit` objects (`plot`, `fitted`,
  `residuals`, `coef`).
- Refactored the shipped Stan model into modular include files (“Stan
  lego bricks”) to simplify extension.

## vrcmort 0.0.2

- Introduced a canonical VR long format contract with explicit
  `exposure`.
- Added data validation (`vrc_validate_data`) and indexing
  (`vrc_index`).
- Added lightweight reporting diagnostics (`vrc_diagnose_reporting`).
- Updated the Stan model and plotting helpers to use `exposure` (with
  `pop` as a fallback).

## vrcmort 0.0.1

- Initial package skeleton.
- Base Stan model for VR mortality with an explicit reporting process.
- Simulator with multiple missingness mechanisms for stress testing.
