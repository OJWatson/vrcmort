# vrcmort: conflict mortality estimation with explicit VR reporting collapse

## Purpose of this repository

This repository contains an R package, `vrcmort`, for fitting Bayesian hierarchical models that estimate **true mortality** from **vital registration (VR)** data when the **observation process changes during conflict**.

The motivating pathology is:

- Pre-conflict VR behaves like a reasonable (if imperfect) measurement of deaths.
- Post-conflict, VR can show:
  - a spike in trauma deaths (plausible),
  - a sharp drop in non-trauma deaths (often implausible as true mortality),
  - an age-at-death distribution that becomes unnaturally young (especially missing older non-trauma deaths),
  - and strong regional variation in reporting breakdown.

If we fit a standard hierarchical small-area model that treats observed VR counts as the truth with noise, the model may incorrectly infer that increasing conflict reduces non-trauma (indirect) mortality. This is an artefact caused by **under-registration and selective reporting**.

`vrcmort` addresses this by explicitly modelling two linked layers:

1) a **latent mortality process** (what deaths truly occurred), and  
2) a **reporting process** (what fraction is observed and recorded in VR, potentially in an age- and cause-selective way).

The package aims to be:
- simple to use for non-expert users (epidemia-like interface),
- extensible for advanced users (modular Stan "lego bricks" and custom Stan support),
- testable and reliable (simulation and benchmarking, strong checks, and CI).

---

## Data setting and practical workflow

### Typical raw data
- Individual death records with:
  - region (admin unit),
  - month of death,
  - age and sex,
  - ICD-10 code (sometimes missing),
  - plus metadata (place of death, facility, etc).

### Typical aggregation for modelling
We usually aggregate to counts by:
- region `r`,
- time `t` (monthly index),
- age group `a`,
- sex `s`,
- cause group `g` (at minimum: trauma vs non-trauma).

ICD-10 codes can be mapped into cause groups (for example trauma vs non-trauma). The model is built to start with two groups and later extend.

### Population denominators and displacement
We assume the user can provide:
- region-month population and demography,
- approximate displacement/migration flows by month that update denominators.

The model uses an **exposure** term, typically:
- `exposure = pop` for monthly bins, or
- `exposure = pop * days_in_month` for person-days, or
- `exposure = pop * coverage_fraction` if only partial time coverage is credible.

---

## Canonical VR long format

The package operates on a canonical "VR long" table where each row is one cell `(r, t, a, s, g)`.

Required columns:
- `region`: region identifier (character or factor)
- `time`: integer time index (1..T, usually months)
- `age`: integer age group index (1..A) or a factor that can be indexed
- `sex`: sex label or index (1..S)
- `cause`: cause group label or index (1..G; typically 1=trauma, 2=non-trauma)
- `y`: observed VR death count (non-negative integer; may be NA if missing)
- `exposure`: modelling denominator (positive numeric)

Recommended columns:
- `pop`: population estimate (optional if exposure already supplied)
- covariates by region-time (repeated within region-time across age/sex/cause cells):
  - `conflict`: conflict intensity (standardised numeric)
  - `facility`: health system functioning proxy (standardised numeric)
  - additional domain covariates (WASH, food insecurity, outbreaks, etc)
- optional data quality measures:
  - `quality_score`, `icd_garbage_share`, `age_missing_share`, etc

---

## The statistical model

### Indices
- `r = 1..R` regions
- `t = 1..T` time points (months)
- `a = 1..A` age groups
- `s = 1..S` sexes
- `g = 1..G` cause groups (baseline implementation focuses on G=2)

### Observed and latent variables
Observed VR counts:
- `Y_{r,t,a,s,g}`

Exposure (denominator):
- `E_{r,t,a,s}` (stored as `exposure` in the data; repeated across cause groups)

Latent (true) mortality rate:
- `λ_{r,t,a,s,g} > 0`

Reporting completeness probability:
- `ρ_{r,t,a,s,g} in (0, 1)`

Expected observed count:
- `μ_{r,t,a,s,g} = E_{r,t,a,s} * λ_{r,t,a,s,g} * ρ_{r,t,a,s,g}`

Likelihood:
- baseline is Negative Binomial (NB2) for robustness:
  - `Y ~ NegBin2(μ, φ_g)` with cause-specific dispersion `φ_g`

This is implemented in Stan.

### Mortality submodel (log-linear)
A core log-linear structure is:

`log λ_{r,t,a,s,g} = α_{0,g} + α_{age[a],g} + α_{sex[s],g} + u^{(λ)}_{r,g} + v^{(λ)}_{t,g}
                    + β_{conf,g} * conflict_{r,t} + X^{mort}_{i} β^{mort}_{g} + ...`

Where:
- `u^{(λ)}_{r,g}` is a region random intercept by cause
- `v^{(λ)}_{t,g}` is a national time random walk (RW1) by cause
- `β_{conf,g}` is the (global) conflict effect on true mortality by cause
- `X^{mort}` is a design matrix for user-specified mortality covariates via formula input

Optional partial pooling extensions supported by the package:
- region-varying conflict slopes:
  - `β_{conf,r,g} ~ Normal(β_{conf,g}, σ_{βconf,g})`
- region-specific time deviations:
  - `v^{(λ,reg)}_{r,t,g}` as RW1 deviations around the national trend

### Reporting submodel (logit-linear)
A reporting model:

`logit ρ_{r,t,a,s,g} = κ_{0,g} + κ_{post,g} * post_t + u^{(ρ)}_{r,g} + v^{(ρ)}_{t,g}
                       + γ_{conf,g} * conflict_{r,t} + X^{rep}_{i} γ^{rep}_{g} + age_penalty(a,t,g) + ...`

Where:
- `post_t` indicates post-conflict period (`t >= t0`)
- `κ_{0,g}` is baseline completeness (anchorable with informative priors)
- `κ_{post,g}` is a post-conflict shift in reporting
- `u^{(ρ)}_{r,g}` and `v^{(ρ)}_{t,g}` are region and time effects for reporting
- `γ_{conf,g}` allows conflict to reduce reporting completeness (often negative for non-trauma)
- `X^{rep}` is a design matrix for user-specified reporting covariates

#### Age-selective reporting collapse (non-trauma)
A key mechanism for the motivating pathology is a post-conflict age penalty applied mainly to non-trauma:

`age_penalty(a,t,non) = - δ_a * post_t`

with `δ_a` constrained to be monotone non-decreasing in age.

Implementation detail:
- `δ_1 = 0` for youngest group
- increments `Δ_a >= 0` for `a = 2..A`
- `δ_a = scale * cumulative_sum(Δ)` to allow a smooth monotone penalty

This structure captures the empirical feature that older non-trauma deaths are disproportionately missing after conflict begins.

### Misclassification (future extension)
The simulator supports misclassification between trauma and non-trauma among recorded deaths, via a 2x2 matrix `M`.
The base Stan model may not always include explicit misclassification. It is a planned extension:
- `E[Y_obs,g'] = Σ_g μ_true,g * M_{g -> g'}`

---

## Identifiability and why priors matter

With only VR counts, `λ` and `ρ` are partially confounded because `μ = exposure * λ * ρ`. Therefore we must anchor the model using:

1) Pre-conflict stability
- Priors that keep pre-conflict reporting completeness plausible and not wildly varying.

2) Age patterns
- The age distribution of deaths provides strong information about whether changes are due to reporting vs real mortality.
- Monotone age penalties prevent the model from making arbitrary, non-identifiable age-specific reporting patterns.

3) Covariate separation
- Put epidemiological drivers (food insecurity, WASH, outbreaks) mainly in mortality.
- Put system functioning / access proxies (facility coverage, data quality indicators) mainly in reporting.
- Conflict may appear in both, but should be regularised to avoid “effect swapping”.

---

## The R package interface

The package provides a high-level interface inspired by `epidemia` and `rstanarm`, allowing users to specify model components separately.

### High-level interface: `vrcm()`
The main user-facing function is `vrcm()`, which constructs and fits the model from two key components:

- `vrcm(mortality = ..., reporting = ..., data = ..., t0 = ...)`

The `mortality` and `reporting` components are specified using helper functions.

- **`vrc_mortality(formula, conflict, time)`**: Defines the latent mortality model.
- **`vrc_reporting(formula, conflict, time)`**: Defines the reporting process model.

Each component allows specification of:
- `formula`: A formula for additional covariates (e.g., `~ food_insecurity`).
- `conflict`: How to model the conflict effect.
    - `"fixed"` (default): A single effect across all regions.
    - `"region"`: Partial pooling with region-specific random slopes.
- `time`: How to model time trends.
    - `"national"` (default): A single random walk across all regions.
    - `"region"`: Region-specific random walk deviations around the national trend.

### Key Model Specification Options
The fitting process can be controlled with several key arguments passed through `vrcm()` to the lower-level `vrc_fit()` function:

- **`pre_conflict_reporting`**: Controls the assumption about reporting before the conflict start time `t0`.
    - `"estimate"` (default): Estimate pre-conflict reporting completeness (`rho`) from the data.
    - `"fixed1"`: Assume pre-conflict reporting was 100% complete (`rho = 1`). This is a strong assumption that can help with model stability if pre-conflict data is reliable. This switch selects a Stan model with `_rho1_pre` in the name.

- **`generated_quantities`**: Controls the Stan generated quantities block for performance.
    - `"full"` (default): Computes all posterior quantities, including `log_lik` for model comparison (e.g., `loo`).
    - `"none"`: Skips the generated quantities block for faster sampling. This is useful when only parameter estimates are needed. This switch selects a Stan model with `_nogq` in the name.

- **`prior_PD`**: If `TRUE`, samples from the prior distribution only, ignoring the likelihood. This is useful for prior predictive checks.

### Other Core Functions
- `vrc_simulate()`: Creates simulated VR long datasets to test model performance.
- `vrc_validate_data()`: Checks the integrity of the input data format.
- `vrc_priors()`: Specify or modify priors on model parameters.
- `vrc_fit()`: The lower-level fitting function that `vrcm()` calls internally. It provides more direct control over model specification.

### Custom Stan workflow
Advanced users can extend the package by:
1.  Copying the modular Stan model to a local directory.
2.  Editing a specific `.stan` include file (e.g., the reporting linear predictor).
3.  Fitting the custom model via `vrc_fit(stan_model = "path/to/model.stan")`.
This allows for custom model development while still using the package's data preparation and posterior extraction tools.

---

## Software architecture

### Stan modular layout
Stan code is split into include files, roughly mirroring epidemia:
- functions
- data
- parameters
- transformed parameters
- priors and likelihood
- generated quantities (including `log_lik`)

The base model `.stan` mostly includes these pieces.

### Outputs
Generated quantities should include:
- `lambda_rep`, `rho_rep`, `mu_rep`, `y_rep`, `log_lik`
These are used for:
- posterior summaries
- PPCs
- LOO/WAIC (via `loo`)

---

## Quality assurance requirements

Because Stan errors and data-initialisation issues can be subtle, the package must catch failures early.

Minimum QA expectations:
1) `R CMD check --as-cran` passes cleanly.
2) Vignettes render without failure.
3) Stan syntax and include resolution are tested (not only via vignettes).
4) Data initialisation dimension mismatches are tested:
   - especially edge cases like K_mort=1, K_rep=1.
5) At least one lightweight end-to-end sampling test exists:
   - tiny simulated data
   - small iterations
   - skipped on CRAN but run in CI.

Recommended CI:
- GitHub Actions workflow that runs:
  - `devtools::check(build_vignettes = TRUE)`
  - minimal Stan parse/compile test
  - unit tests

---

## Near-term roadmap (what “done” looks like)

1) Robust build and check
- Clean docs, imports, NAMESPACE.
- Tests cover Stan parse/compile and standata shape edge cases.
- Vignettes pass.

2) User experience parity with epidemia
- Simple model specification.
- Clear, slow, mathematical documentation.
- “Do the right thing by default” priors and diagnostics.

3) Core extensions
- Misclassification in Stan (not only simulator).
- Optional ill-defined cause group.
- Optional population uncertainty (latent multiplier on exposure).
- Optional spatial structure (CAR/BYM2) if needed.

---

## Key modelling takeaways (why this package exists)

- The central challenge is not missing region-time cells, it is that the observation mechanism changes with conflict.
- By modelling reporting explicitly, the package prevents common sign-flip artefacts in indirect mortality.
- Simulation and strong tests are essential. If you cannot recover truth on simulated data with known reporting collapse, you cannot trust real-world inference.
