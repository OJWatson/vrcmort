# Gemini Code Assistant Project Configuration: vrcmort

## 1. Project Overview and Core Purpose

`vrcmort` is an R package for fitting Bayesian hierarchical models that estimate **true mortality** from **vital registration (VR)** data, specifically when the observation process is disrupted by conflict.

The core problem it solves is the misinterpretation of VR data during crises. Conflict can cause a drop in reported non-trauma deaths, which is an artefact of **under-registration and selective reporting**, not a true decrease in mortality.

`vrcmort` addresses this by explicitly modelling two distinct processes:
1.  A **latent mortality process** (the true, unobserved deaths).
2.  A **reporting process** (the fraction of true deaths that are actually observed and recorded).

The package is designed to be simple for standard use cases, extensible for expert users, and robustly tested.

## 2. Key Files and Codebase Structure

The project follows a standard R package structure.

-   **`R/`**: Main R source code.
    -   `interface.R`: Core user-facing functions like `vrcm()` and `vrc_fit()`.
    -   `fit.R`: Logic for fitting the Stan models using `rstan`.
    -   `standata.R`: Functions for preparing data into the list format required by Stan.
    -   `priors.R`: Handling of prior specifications.
    -   `simulate.R`: The `vrc_simulate()` function for creating test data.
    -   `posterior.R`: Functions for extracting and summarizing posterior quantities.
    -   `plots.R`: Plotting functions like `plot_mortality()` and `plot_reporting()`.
-   **`inst/stan/`**: Modular Stan models.
    -   The main model files (`vr_reporting_model.stan`) are composed of smaller, included parts from subdirectories (`model/`, `parameters/`, `priors/`, etc.). This mirrors the structure of the `epidemia` package.
    -   This modularity is key for custom extensions.
-   **`tests/testthat/`**: Unit and integration tests using the `testthat` framework.
    -   `test-fit-tiny-sampling.R`: An important end-to-end test that fits a model on simulated data.
    -   `test-standata.R`: Tests for the data preparation logic.
    -   `test-stan-parse.R`: Ensures Stan models compile correctly.
-   **`vignettes/`**: Long-form documentation, tutorials, and mathematical model descriptions. These are critical for understanding the model's implementation and use.
-   **`man/`**: Generated R documentation files.
-   **`DESCRIPTION`**: Defines package metadata, dependencies (`rstan`, `loo`), and links to the Stan models (`StanHeaders`).

## 3. Development Workflow and Technical Standards

### R Development
-   **Activate relevant sub-agents or skills**: Adhere to modern R development practices, particularly Tidyverse style and `rlang` conventions where applicable.
-   **Dependencies**: The project is built on `rstan`. Familiarity with `rstan` objects (`stanfit`) is essential.
-   **User Interface**: The primary user-facing function is the `epidemia`-style wrapper `vrcm()`. It takes `vrc_mortality()` and `vrc_reporting()` components. Changes should maintain this high-level interface. The lower-level `vrc_fit()` function is called internally.

### Key Model Switches and Controls
The `vrcm()` and `vrc_fit()` functions provide several critical controls for model specification:

-   **Partial Pooling Control**: The `vrc_mortality` and `vrc_reporting` components accept `conflict` and `time` arguments to control random effects structure.
    -   `conflict = "region"` enables region-specific random slopes for the conflict effect.
    -   `time = "region"` enables region-specific random walk deviations from the national trend.

-   **Pre-Conflict Reporting Assumption (`pre_conflict_reporting`)**: This is a crucial switch passed to `vrc_fit()`.
    -   `"estimate"` (default): The model estimates pre-conflict reporting completeness `rho`.
    -   `"fixed1"`: The model assumes `rho = 1` (100% completeness) before `t0`. This selects the `_rho1_pre.stan` model variant and is a strong, identifying assumption.

-   **Performance Toggle (`generated_quantities`)**:
    -   `"full"` (default): Runs the full generated quantities block in Stan.
    -   `"none"`: Uses a `_nogq.stan` model variant that skips generated quantities for faster sampling.

### Data Format: Canonical "VR Long"
The package expects a specific long-format data frame. Each row represents a single stratum `(region, time, age, sex, cause)`.
-   **Required columns**: `region`, `time`, `age`, `sex`, `cause`, `y` (observed counts), `exposure`.
-   **Action**: Before processing data, always validate it using `vrc_validate_data()`.

### Statistical Model
The model has two main components defined in the Stan files:
1.  **Mortality (log-linear)**: `log(λ) = ... + β_mort * covariates`
2.  **Reporting (logit-linear)**: `logit(ρ) = ... + γ_rep * covariates`

-   **Identifiability**: The model is only identifiable because of the structure imposed by priors and covariate effects. Be extremely careful when modifying priors or the model structure. The `pre_conflict_reporting = "fixed1"` switch is a key tool for improving identifiability.
-   **Priors**: Priors are crucial for anchoring the model. The function `vrc_priors()` is used to specify them.
-   **Covariates**: A key assumption is the separation of covariates:
    -   Epidemiological drivers (e.g., food insecurity) primarily affect the **mortality** model (`λ`).
    -   System functioning proxies (e.g., facility access) primarily affect the **reporting** model (`ρ`).

### Stan Development
-   **Modularity**: When modifying the Stan model, edit the specific include files in `inst/stan/` rather than the main `.stan` file.
-   **Custom Models**: The workflow `vrc_fit(stan_model = "path/to/custom.stan")` allows advanced users to supply their own models while reusing the package's data preparation and posterior processing tools.

### Quality Assurance and Testing
The project has strict quality standards.
1.  **`R CMD check --as-cran`**: Must pass cleanly.
2.  **Vignette Rendering**: All vignettes must build successfully.
3.  **Testing**:
    -   Any new R function must have corresponding tests in `tests/testthat/`.
    -   Changes to Stan code must be validated by the parsing tests (`test-stan-parse.R`) and the lightweight sampling test (`test-fit-tiny-sampling.R`).
    -   The CI workflows in `.github/workflows/` are the source of truth for testing procedures.

## 4. Roadmap and Future Extensions

The near-term goal is to achieve parity with `epidemia` for user experience and robustness. Key planned extensions include:
-   Adding explicit **misclassification** between cause groups to the Stan model.
-   Support for an **ill-defined cause** group.
-   Modelling **population uncertainty**.
-   Adding optional **spatial smoothing** (e.g., CAR/BYM2) for regional random effects.

When implementing new features, consider if they fit within this roadmap.
