# Package index

## Model fitting

- [`vrc_fit()`](https://github.com/OJWatson/vrcmort/reference/vrc_fit.md)
  : Fit the VR reporting model
- [`vrcm()`](https://github.com/OJWatson/vrcmort/reference/vrcm.md) :
  Fit a VR mortality + reporting model
- [`vrc_standata()`](https://github.com/OJWatson/vrcmort/reference/vrc_standata.md)
  : Build Stan data for the VR reporting model
- [`vrc_model()`](https://github.com/OJWatson/vrcmort/reference/vrc_model.md)
  : Load and cache a Stan model
- [`vrc_mortality()`](https://github.com/OJWatson/vrcmort/reference/vrc_mortality.md)
  : Mortality model component specification
- [`vrc_reporting()`](https://github.com/OJWatson/vrcmort/reference/vrc_reporting.md)
  : Reporting model component specification
- [`vrc_priors()`](https://github.com/OJWatson/vrcmort/reference/vrc_priors.md)
  : Prior specification constructors

## Data preparation

- [`vrc_validate_data()`](https://github.com/OJWatson/vrcmort/reference/vrc_validate_data.md)
  : Validate a VR long-format dataset
- [`vrc_index()`](https://github.com/OJWatson/vrcmort/reference/vrc_index.md)
  : Index a VR long-format dataset
- [`vrc_diagnose_reporting()`](https://github.com/OJWatson/vrcmort/reference/vrc_diagnose_reporting.md)
  : Diagnose VR reporting artefacts
- [`vrc_canonical_columns()`](https://github.com/OJWatson/vrcmort/reference/vrc_canonical_columns.md)
  : Canonical VR long format

## Simulation

- [`vrc_simulate()`](https://github.com/OJWatson/vrcmort/reference/vrc_simulate.md)
  : Simulate VR mortality with conflict-driven reporting collapse
- [`vrc_sim_truth()`](https://github.com/OJWatson/vrcmort/reference/vrc_sim_truth.md)
  : Extract cell-level truth from a simulated dataset

## Posterior extraction

- [`posterior_reporting()`](https://github.com/OJWatson/vrcmort/reference/posterior_reporting.md)
  : Posterior summaries for reporting completeness
- [`posterior_mortality()`](https://github.com/OJWatson/vrcmort/reference/posterior_mortality.md)
  : Posterior summaries for latent mortality rates
- [`posterior_expected_counts()`](https://github.com/OJWatson/vrcmort/reference/posterior_expected_counts.md)
  : Posterior summaries for expected observed counts
- [`posterior_predict()`](https://github.com/OJWatson/vrcmort/reference/posterior_predict.md)
  : Posterior predictive distribution for observed counts
- [`vrc_prior_summary()`](https://github.com/OJWatson/vrcmort/reference/vrc_prior_summary.md)
  : Summarise priors used by a fitted model
- [`vrc_coef_summary()`](https://github.com/OJWatson/vrcmort/reference/vrc_coef_summary.md)
  : Summarise covariate effects
- [`vrc_conflict_effects()`](https://github.com/OJWatson/vrcmort/reference/vrc_conflict_effects.md)
  : Region-specific conflict effects

## Building new models

- [`exponential()`](https://github.com/OJWatson/vrcmort/reference/exponential.md)
  : Exponential prior
- [`normal()`](https://github.com/OJWatson/vrcmort/reference/normal.md)
  : Normal prior
- [`vrc_model_names()`](https://github.com/OJWatson/vrcmort/reference/vrc_model_names.md)
  : List Stan models shipped with the package
- [`vrc_model_spec()`](https://github.com/OJWatson/vrcmort/reference/vrc_model_spec.md)
  : Define a model specification
- [`vrc_write_stan_template()`](https://github.com/OJWatson/vrcmort/reference/vrc_write_stan_template.md)
  : Write a custom Stan template to a directory

## Plots

- [`plot_reporting()`](https://github.com/OJWatson/vrcmort/reference/plot_reporting.md)
  : Plot estimated reporting completeness over time
- [`plot_mortality()`](https://github.com/OJWatson/vrcmort/reference/plot_mortality.md)
  : Plot estimated mortality over time
