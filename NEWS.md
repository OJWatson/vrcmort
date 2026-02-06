# vrcmort 0.0.9

- Added support for handling missing region labels in VR counts using a Missing At Random (MAR) assumption.
- Implemented an exact Negative Binomial convolution likelihood in Stan for marginalized inference over unlabeled counts.
- Added `omega` parameter for the labeling probability with a default `beta(5, 2)` prior.
- New `use_mar_labels` toggle in `vrcm()`, `vrc_fit()`, and `vrc_standata()` (off by default).
- Updated `vrc_simulate()` to support generating data with missing labels.

# vrcmort 0.0.8

- pkgdown website referencing completed
- Gihtub CI completed

# vrcmort 0.0.7

- Added support for user-supplied Stan files via `vrc_model_spec()` and `vrc_write_stan_template()`.
- Posterior extraction helpers now respect a `param_map` so generated quantity names can be customised.
- Added basic CI configuration and improved R CMD check hygiene (`.Rbuildignore`, global variable declarations).
- Minor test updates for testthat edition 3.

# vrcmort 0.0.6

- Expanded documentation with epidemia-style vignettes covering the model introduction, mathematical description, implementation details, schematic, partial pooling, priors, and under-reporting with age-selective reporting collapse.
- Added two step-by-step tutorial vignettes (data preparation from individual records and covariate exploration).
- Updated pkgdown configuration to surface the new articles.

# vrcmort 0.0.5

- Added a lightweight prior specification system (`vrc_priors()`) inspired by rstanarm/epidemia.
- Users can now override key priors (covariate coefficients, conflict effects, reporting anchors, random-effect scales, dispersion) via `vrcm()` / `vrc_fit()`.
- Stan model priors are now parameterised through Stan `data` (hyperparameters) rather than hard-coded constants.

# vrcmort 0.0.4

- Added optional partial pooling for conflict effects by region (random slopes) in both the mortality and reporting components.
- Added optional region-specific time random walks (deviations around the national trend) for both mortality and reporting.
- Added helpers to extract region-specific conflict effects from fitted models.
- Fixed `vrc_fit()` to pass `duplicates` through to `vrc_standata()`.

# vrcmort 0.0.3

- Added an epidemia-style high-level interface (`vrcm`) with explicit mortality and reporting components.
- Added convenience S3 methods for `vrcfit` objects (`plot`, `fitted`, `residuals`, `coef`).
- Refactored the shipped Stan model into modular include files ("Stan lego bricks") to simplify extension.

# vrcmort 0.0.2

- Introduced a canonical VR long format contract with explicit `exposure`.
- Added data validation (`vrc_validate_data`) and indexing (`vrc_index`).
- Added lightweight reporting diagnostics (`vrc_diagnose_reporting`).
- Updated the Stan model and plotting helpers to use `exposure` (with `pop` as a fallback).

# vrcmort 0.0.1

- Initial package skeleton.
- Base Stan model for VR mortality with an explicit reporting process.
- Simulator with multiple missingness mechanisms for stress testing.
