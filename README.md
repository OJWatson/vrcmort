# vrcmort

[Deep Wiki Website](https://deepwiki.com/OJWatson/vrcmort)

`vrcmort` provides tools for fitting hierarchical Bayesian models to vital
registration (VR) mortality counts when the VR reporting mechanism changes over
space and time (for example, during armed conflict).

The core model separates:

- a **latent mortality process** (true deaths), and
- an **observation / reporting process** (registration completeness and
  age-selective reporting collapse).

This allows conflict to increase true mortality while observed non-trauma VR
counts can decline because reporting collapses.

## Installation

This repository is structured as an R package. For local development:

```r
# install.packages("devtools")
devtools::install("path/to/vrcmort")
```

The package uses Stan via `rstan`. You will need a working C++ toolchain and a
Stan-capable R setup.

## Quick start

```r
library(vrcmort)

# 1) Simulate data
sim <- vrc_simulate(T = 96, t0 = 40, seed = 123)

# Optional: diagnose reporting artefacts before fitting
diag <- vrc_diagnose_reporting(sim$df_obs, t0 = sim$meta$t0)
# diag$plots$by_cause

# 2) Build Stan data and fit
fit <- vrc_fit(
  data = sim$df_obs,
  t0 = sim$meta$t0,
  mortality_covariates = ~ facility,   # example
  reporting_covariates = ~ facility,   # example
  algorithm = "sampling",
  chains = 4,
  iter = 1000,
  seed = 123
)

# Or, using the high-level interface:
mort <- vrc_mortality(~ facility)
rep <- vrc_reporting(~ facility)

# Optional: allow the conflict effect to vary by region (partial pooling)
# and add region-specific time trends around the national random walk.
# This can help prevent spurious "conflict reduces mortality" artefacts when
# reporting collapses differently across regions.
mort2 <- vrc_mortality(~ facility, conflict = "region", time = "region")
rep2  <- vrc_reporting(~ facility, conflict = "region", time = "region")

fit2 <- vrcm(
  mortality = mort,
  reporting = rep,
  data = sim$df_obs,
  t0 = sim$meta$t0,
  chains = 4,
  iter = 1000,
  seed = 123
)

fit3 <- vrcm(
  mortality = mort2,
  reporting = rep2,
  data = sim$df_obs,
  t0 = sim$meta$t0,
  chains = 4,
  iter = 1000,
  seed = 123
)

# 3) Summarise and plot reporting completeness
summary(fit)
plot_reporting(fit)
```

## Package status

This is a research-facing package skeleton intended for iterative development.
It is designed to mirror the modelling workflow of packages like `epidemia` and 
the feel of `rstanarm`, but with a focus on VR mortality and reporting collapse.

## Vignettes

Model series:

- *Model Introduction* (`model-introduction`)
- *Model Description* (`model-description`)
- *Model Implementation* (`model-implementation`)
- *Model Schematic* (`model-schematic`)
- *Partial Pooling* (`partial-pooling`)
- *Priors* (`priors`)
- *Under-reporting and Age Structure* (`underreporting-age`)

Tutorials:

- *Tutorial: preparing VR long data from individual records* (`tutorial-data-prep`)
- *Tutorial: exploring covariates and pooling* (`tutorial-covariates-pooling`)
- *Stress testing under different missingness mechanisms* (`missingness`)
